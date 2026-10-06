/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_ir_image_INCLUDED
#define dsl_ir_image_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opcode.h"
#include "srcpos.h"
#include "symtab_idx.h"
#include "targ_const.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/*
 * Source-level DSL fixed-row image model.
 *
 * These records use the existing ir_bread.cxx and ir_bwrite.cxx mapped-image
 * path when at least one DSL row is present.  Legacy images omit the optional
 * section and load as an empty DSL image.
 *
 * Binary artifact work should extend the existing mapped-image / ELF WHIRL
 * path.  Do not introduce a Python-owned or torch2whirl-specific side format
 * for compiler IR.
 */

#define DSL_IR_IMAGE_VERSION 1

#define DSL_IR_IMAGE_HEADER_SIZE             48
#define DSL_IR_OPCODE_DESCRIPTOR_RECORD_SIZE 72
#define DSL_IR_NODE_RECORD_SIZE              40
#define DSL_IR_ATTRIBUTE_RECORD_SIZE         32
#define DSL_IR_VALUE_RECORD_SIZE             48
#define DSL_IR_VALUE_REFERENCE_RECORD_SIZE   24

#define DSL_EFFECT_IMAGE_MAGIC              0x44534c45
#define DSL_EFFECT_IMAGE_VERSION            1
#define DSL_EFFECT_IMAGE_HEADER_SIZE        24
#define DSL_STATE_OBJECT_RECORD_SIZE        40
#define DSL_STATE_EFFECT_RECORD_SIZE        24

#define DSL_CALL_IMAGE_MAGIC                0x44534c43
#define DSL_CALL_IMAGE_VERSION              1
#define DSL_CALL_IMAGE_HEADER_SIZE          24
#define DSL_PU_SOURCE_IDENTITY_RECORD_SIZE  48
#define DSL_CALLSITE_METADATA_RECORD_SIZE   48

#define DSL_CALL_ABI_IMAGE_MAGIC            0x44534142
#define DSL_CALL_ABI_IMAGE_VERSION          1
#define DSL_CALL_ABI_IMAGE_HEADER_SIZE      24
#define DSL_CALL_ARGUMENT_RECORD_SIZE       32

#define DSL_PU_INTERFACE_IMAGE_MAGIC        0x44535049
#define DSL_PU_INTERFACE_IMAGE_VERSION      1
#define DSL_PU_INTERFACE_IMAGE_HEADER_SIZE  24
#define DSL_PU_FORMAL_RECORD_SIZE           32

#define DSL_RUNTIME_INTERFACE_IMAGE_MAGIC       0x44535249
#define DSL_RUNTIME_INTERFACE_IMAGE_VERSION     1
#define DSL_RUNTIME_INTERFACE_IMAGE_HEADER_SIZE 32
#define DSL_RUNTIME_VALUE_PROJECTION_RECORD_SIZE 48
#define DSL_RUNTIME_CALL_PROJECTION_RECORD_SIZE  40

#define DSL_PROGRAM_INTERFACE_IMAGE_MAGIC       0x44535047
#define DSL_PROGRAM_INTERFACE_IMAGE_VERSION     1
#define DSL_PROGRAM_INTERFACE_IMAGE_HEADER_SIZE 64
#define DSL_RETIRED_FORMAL_RECORD_SIZE           48
#define DSL_RETIRED_CALL_ARGUMENT_RECORD_SIZE    48
#define DSL_RUNTIME_INPUT_RECORD_SIZE            64
#define DSL_RUNTIME_INPUT_BINDING_RECORD_SIZE    48
#define DSL_RUNTIME_INPUT_CALL_RECORD_SIZE       48

#define DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID 0
#define DSL_IR_NODE_INVALID_ID              0
#define DSL_IR_ATTRIBUTE_INVALID_ID         0
#define DSL_IR_VALUE_INVALID_ID             0
#define DSL_IR_VALUE_REFERENCE_INVALID_ID   0

typedef UINT32 DSL_IR_OPCODE_DESCRIPTOR_ID;
typedef UINT32 DSL_IR_NODE_ID;
typedef UINT32 DSL_IR_ATTRIBUTE_ID;
typedef UINT32 DSL_IR_VALUE_ID;
typedef UINT32 DSL_IR_VALUE_REFERENCE_ID;
typedef UINT32 DSL_STATE_OBJECT_ID;
typedef UINT32 DSL_STATE_EFFECT_ID;
typedef UINT32 DSL_PU_SOURCE_IDENTITY_ID;
typedef UINT32 DSL_CALLSITE_METADATA_ID;
typedef UINT32 DSL_CALL_ARGUMENT_ID;
typedef UINT32 DSL_PU_FORMAL_ID;
typedef UINT32 DSL_RUNTIME_VALUE_PROJECTION_ID;
typedef UINT32 DSL_RUNTIME_CALL_PROJECTION_ID;
typedef UINT32 DSL_RETIRED_FORMAL_ID;
typedef UINT32 DSL_RETIRED_CALL_ARGUMENT_ID;
typedef UINT32 DSL_RUNTIME_INPUT_ID;
typedef UINT32 DSL_RUNTIME_INPUT_BINDING_ID;
typedef UINT32 DSL_RUNTIME_INPUT_CALL_ID;

#define DSL_STATE_OBJECT_INVALID_ID 0
#define DSL_STATE_EFFECT_INVALID_ID 0
#define DSL_PU_SOURCE_IDENTITY_INVALID_ID 0
#define DSL_CALLSITE_METADATA_INVALID_ID 0
#define DSL_CALL_ARGUMENT_INVALID_ID 0
#define DSL_CALL_ARGUMENT_INVALID_ORDINAL ((UINT32)-1)
#define DSL_PU_FORMAL_INVALID_ID 0
#define DSL_PU_FORMAL_INVALID_ORDINAL ((UINT32)-1)
#define DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID 0
#define DSL_RUNTIME_CALL_PROJECTION_INVALID_ID 0
#define DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ((UINT32)-1)
#define DSL_RETIRED_FORMAL_INVALID_ID 0
#define DSL_RETIRED_CALL_ARGUMENT_INVALID_ID 0
#define DSL_RUNTIME_INPUT_INVALID_ID 0
#define DSL_RUNTIME_INPUT_BINDING_INVALID_ID 0
#define DSL_RUNTIME_INPUT_CALL_INVALID_ID 0

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 pu_identity_count;
    UINT32 callsite_count;
    UINT32 flags;
    UINT32 reserved;
} DSL_CALL_IMAGE_HEADER;

typedef struct {
    DSL_PU_SOURCE_IDENTITY_ID id;
    UINT32 flags;
    ST_IDX owner_pu_st;
    STR_IDX canonical_definition_name;
    STR_IDX defining_module;
    STR_IDX defining_file;
    UINT32 defining_line;
    UINT32 reserved;
} DSL_PU_SOURCE_IDENTITY_RECORD;

typedef struct {
    DSL_CALLSITE_METADATA_ID id;
    UINT32 wn_offset;
    ST_IDX owner_pu_st;
    ST_IDX callee_pu_st;
    STR_IDX canonical_class_name;
    STR_IDX instance_path;
    STR_IDX context_identity;
    UINT32 source_call_ordinal;
    UINT32 flags;
} DSL_CALLSITE_METADATA_RECORD;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 argument_count;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_CALL_ABI_IMAGE_HEADER;

typedef struct {
    DSL_CALL_ARGUMENT_ID id;
    DSL_CALLSITE_METADATA_ID callsite_id;
    DSL_IR_VALUE_ID argument_value_id;
    UINT32 actual_ordinal;
    UINT32 callee_formal_ordinal;
    UINT32 flags;
    STR_IDX semantic_role;
} DSL_CALL_ARGUMENT_RECORD;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 formal_count;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_PU_INTERFACE_IMAGE_HEADER;

typedef struct {
    DSL_PU_FORMAL_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID formal_value_id;
    UINT32 formal_ordinal;
    ST_IDX formal_st;
    TY_IDX formal_ty;
    UINT32 flags;
    UINT32 reserved;
} DSL_PU_FORMAL_RECORD;

typedef enum {
    DSL_RUNTIME_BINDING_UNKNOWN = 0,
    DSL_RUNTIME_BINDING_LOCAL_VALUE = 1,
    DSL_RUNTIME_BINDING_INPUT_FORMAL = 2,
    DSL_RUNTIME_BINDING_RESULT_FORMAL = 3
} DSL_RUNTIME_BINDING_KIND;

typedef enum {
    DSL_RUNTIME_CALL_UNKNOWN = 0,
    DSL_RUNTIME_CALL_INPUT = 1,
    DSL_RUNTIME_CALL_RESULT = 2
} DSL_RUNTIME_CALL_DIRECTION;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 value_projection_count;
    UINT32 call_projection_count;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_RUNTIME_INTERFACE_IMAGE_HEADER;

typedef struct {
    DSL_RUNTIME_VALUE_PROJECTION_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    ST_IDX source_st;
    TY_IDX source_ty;
    ST_IDX handle_st;
    TY_IDX handle_ty;
    UINT32 binding_kind;
    UINT32 formal_ordinal;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_RUNTIME_VALUE_PROJECTION_RECORD;

typedef struct {
    DSL_RUNTIME_CALL_PROJECTION_ID id;
    ST_IDX owner_pu_st;
    DSL_CALLSITE_METADATA_ID callsite_id;
    DSL_RUNTIME_VALUE_PROJECTION_ID value_projection_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 actual_ordinal;
    UINT32 callee_formal_ordinal;
    UINT32 direction;
    UINT32 flags;
    UINT32 reserved;
} DSL_RUNTIME_CALL_PROJECTION_RECORD;

typedef enum {
    DSL_INTERFACE_RETIREMENT_UNKNOWN = 0,
    DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT = 1
} DSL_INTERFACE_RETIREMENT_REASON;

typedef enum {
    DSL_RUNTIME_INPUT_UNKNOWN = 0,
    DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR = 1,
    DSL_RUNTIME_INPUT_OPAQUE_RESOURCE = 2,
    DSL_RUNTIME_INPUT_TENSOR_TCON_RESOURCE = 3
} DSL_RUNTIME_INPUT_KIND;

typedef enum {
    DSL_RUNTIME_INPUT_BINDING_UNKNOWN = 0,
    DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE = 1,
    DSL_RUNTIME_INPUT_BINDING_ROOT_RESOURCE = 2,
    DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL = 3
} DSL_RUNTIME_INPUT_BINDING_KIND;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 retired_formal_count;
    UINT32 retired_call_argument_count;
    UINT32 runtime_input_count;
    UINT32 runtime_input_binding_count;
    UINT32 runtime_input_call_count;
    UINT32 flags;
    UINT32 reserved[8];
} DSL_PROGRAM_INTERFACE_IMAGE_HEADER;

typedef struct {
    DSL_RETIRED_FORMAL_ID id;
    DSL_PU_FORMAL_ID pu_formal_id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID formal_value_id;
    ST_IDX formal_st;
    TY_IDX formal_ty;
    UINT32 old_formal_ordinal;
    UINT32 retirement_reason;
    STR_IDX semantic_role;
    UINT32 flags;
    UINT32 reserved;
} DSL_RETIRED_FORMAL_RECORD;

typedef struct {
    DSL_RETIRED_CALL_ARGUMENT_ID id;
    DSL_CALL_ARGUMENT_ID call_argument_id;
    DSL_CALLSITE_METADATA_ID callsite_id;
    DSL_IR_VALUE_ID argument_value_id;
    UINT32 old_actual_ordinal;
    UINT32 old_callee_formal_ordinal;
    UINT32 retirement_reason;
    UINT32 flags;
    STR_IDX semantic_role;
    UINT32 reserved[2];
} DSL_RETIRED_CALL_ARGUMENT_RECORD;

typedef struct {
    DSL_RUNTIME_INPUT_ID id;
    UINT32 input_kind;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    ST_IDX source_st;
    TY_IDX source_ty;
    TCON_IDX source_tcon;
    UINT32 flags;
    STR_IDX stable_role;
    TY_IDX handle_ty;
    UINT32 reserved[5];
} DSL_RUNTIME_INPUT_RECORD;

typedef struct {
    DSL_RUNTIME_INPUT_BINDING_ID id;
    ST_IDX owner_pu_st;
    DSL_RUNTIME_INPUT_ID runtime_input_id;
    ST_IDX handle_st;
    TY_IDX handle_ty;
    UINT32 final_formal_ordinal;
    UINT32 binding_kind;
    UINT32 flags;
    STR_IDX semantic_role;
    UINT32 reserved[2];
} DSL_RUNTIME_INPUT_BINDING_RECORD;

typedef struct {
    DSL_RUNTIME_INPUT_CALL_ID id;
    DSL_CALLSITE_METADATA_ID callsite_id;
    ST_IDX caller_owner_pu_st;
    UINT32 caller_final_formal_ordinal;
    ST_IDX callee_owner_pu_st;
    UINT32 callee_final_formal_ordinal;
    UINT32 final_actual_ordinal;
    UINT32 final_callee_formal_ordinal;
    TY_IDX handle_ty;
    UINT32 flags;
    STR_IDX semantic_role;
} DSL_RUNTIME_INPUT_CALL_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    ST_IDX expected_source_st;
    TY_IDX expected_source_ty;
    TY_IDX handle_ty;
    UINT32 binding_kind;
    UINT32 formal_ordinal;
} DSL_RUNTIME_VALUE_PROJECTION_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    DSL_CALLSITE_METADATA_ID callsite_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 actual_ordinal;
    UINT32 callee_formal_ordinal;
    UINT32 direction;
} DSL_RUNTIME_CALL_PROJECTION_REQUEST;

typedef struct {
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *values;
    UINT32 value_count;
    const DSL_RUNTIME_CALL_PROJECTION_REQUEST *calls;
    UINT32 call_count;
} DSL_RUNTIME_INTERFACE_PLAN;

typedef struct {
    UINT32 value_projection_count;
    UINT32 call_projection_count;
    UINT32 rebuilt_formal_count;
    UINT32 rewritten_call_count;
    UINT32 rewritten_return_count;
} DSL_RUNTIME_INTERFACE_RESULT;

#define DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX ((UINT32)-1)

typedef struct {
    DSL_PU_FORMAL_ID pu_formal_id;
    const char *semantic_role;
} DSL_RETIRED_FORMAL_REQUEST;

typedef struct {
    DSL_CALL_ARGUMENT_ID call_argument_id;
    const char *semantic_role;
} DSL_RETIRED_CALL_ARGUMENT_REQUEST;

typedef struct {
    UINT32 input_kind;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    TY_IDX source_ty;
    TCON_IDX source_tcon;
    const char *stable_role;
    TY_IDX handle_ty;
} DSL_RUNTIME_INPUT_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    UINT32 runtime_input_index;
    TY_IDX handle_ty;
    UINT32 binding_kind;
    const char *semantic_role;
    SRCPOS source_position;
} DSL_RUNTIME_INPUT_BINDING_REQUEST;

typedef struct {
    DSL_CALLSITE_METADATA_ID callsite_id;
    UINT32 caller_binding_index;
    UINT32 callee_binding_index;
} DSL_RUNTIME_INPUT_CALL_REQUEST;

typedef struct {
    const DSL_RETIRED_FORMAL_REQUEST *retired_formals;
    UINT32 retired_formal_count;
    const DSL_RETIRED_CALL_ARGUMENT_REQUEST *retired_call_arguments;
    UINT32 retired_call_argument_count;
    const DSL_RUNTIME_INPUT_REQUEST *runtime_inputs;
    UINT32 runtime_input_count;
    const DSL_RUNTIME_INPUT_BINDING_REQUEST *runtime_input_bindings;
    UINT32 runtime_input_binding_count;
    const DSL_RUNTIME_INPUT_CALL_REQUEST *runtime_input_calls;
    UINT32 runtime_input_call_count;
} DSL_PROGRAM_INTERFACE_PLAN;

typedef struct {
    UINT32 retired_formal_count;
    UINT32 retired_call_argument_count;
    UINT32 runtime_input_count;
    UINT32 runtime_binding_count;
    UINT32 runtime_call_count;
    UINT32 canonical_projection_count;
    UINT32 rewritten_call_count;
    UINT32 rewritten_return_count;
} DSL_PROGRAM_INTERFACE_RESULT;

typedef enum {
    DSL_IR_IMAGE_RECORD_UNKNOWN = 0,
    DSL_IR_IMAGE_RECORD_TENSOR_DESCRIPTOR = 1,
    DSL_IR_IMAGE_RECORD_TENSOR_TYPE_CORE = 2,
    DSL_IR_IMAGE_RECORD_TENSOR_TRAIT_SET = 3,
    DSL_IR_IMAGE_RECORD_TENSOR_REPRESENTATION = 4,
    DSL_IR_IMAGE_RECORD_TENSOR_LINEAGE = 5,
    DSL_IR_IMAGE_RECORD_OPCODE_DESCRIPTOR = 6,
    DSL_IR_IMAGE_RECORD_NODE = 7,
    DSL_IR_IMAGE_RECORD_ATTRIBUTE = 8,
    DSL_IR_IMAGE_RECORD_VALUE = 9,
    DSL_IR_IMAGE_RECORD_VALUE_REFERENCE = 10
} DSL_IR_IMAGE_RECORD_KIND;

typedef enum {
    DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR = 0x00000001,
    DSL_IR_IMAGE_CAP_NODE = 0x00000002,
    DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE = 0x00000004,
    DSL_IR_IMAGE_CAP_VALUE = 0x00000008,
    DSL_IR_IMAGE_CAP_VALUE_REFERENCE = 0x00000010
} DSL_IR_IMAGE_CAPABILITY;

typedef struct {
    UINT32 version;
    UINT32 record_kind_count;
    UINT32 flags;
    UINT32 capabilities;
    UINT32 opcode_descriptor_count;
    UINT32 node_count;
    UINT32 attribute_count;
    UINT32 value_count;
    UINT32 value_reference_count;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_IR_IMAGE_HEADER;

/*
 * Fixed rows use only sized scalars, table IDs, symbol/type IDs, and STR_IDX.
 * They must remain POD records with no pointers, STL containers, or ownership.
 */
typedef struct {
    DSL_IR_OPCODE_DESCRIPTOR_ID id;
    UINT32 logical_operator;
    UINT32 version;
    INT32 operand_count;
    UINT32 category;
    UINT32 level;
    UINT32 shape_rule;
    UINT32 effect_model;
    UINT32 lowering_model;
    UINT32 flags;
    STR_IDX logical_name;
    STR_IDX stable_name;
    STR_IDX attribute_schema;
    STR_IDX diagnostic_prefix;
} DSL_IR_OPCODE_DESCRIPTOR_RECORD;

typedef struct {
    DSL_IR_NODE_ID id;
    DSL_IR_OPCODE_DESCRIPTOR_ID opcode_descriptor_id;
    DSL_IR_VALUE_REFERENCE_ID first_operand_reference_id;
    UINT32 operand_count;
    DSL_IR_ATTRIBUTE_ID first_attribute_id;
    UINT32 attribute_count;
    DSL_IR_VALUE_ID result_value_id;
    UINT32 flags;
    STR_IDX payload;
} DSL_IR_NODE_RECORD;

typedef enum {
    DSL_IR_NODE_FLAG_NONE = 0,
    DSL_IR_NODE_FLAG_RETIRED = 0x00000001,
    DSL_IR_NODE_FLAG_LOWERED = 0x00000002,
    DSL_IR_NODE_FLAG_DEAD_ELIDED = 0x00000004,
    DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW = 0x00000008
} DSL_IR_NODE_FLAG;

#define DSL_IR_NODE_REDIRECT_ORDINAL_SHIFT 16
#define DSL_IR_NODE_REDIRECT_ORDINAL_MASK  0xffff0000

typedef enum {
    DSL_IR_ATTRIBUTE_VALUE_UNKNOWN = 0,
    DSL_IR_ATTRIBUTE_VALUE_STRING = 1,
    DSL_IR_ATTRIBUTE_VALUE_SIGNED = 2,
    DSL_IR_ATTRIBUTE_VALUE_UNSIGNED = 3,
    DSL_IR_ATTRIBUTE_VALUE_FLOAT = 4,
    DSL_IR_ATTRIBUTE_VALUE_BOOLEAN = 5,
    DSL_IR_ATTRIBUTE_VALUE_TYPE = 6,
    DSL_IR_ATTRIBUTE_VALUE_SYMBOL = 7
} DSL_IR_ATTRIBUTE_VALUE_KIND;

typedef struct {
    DSL_IR_ATTRIBUTE_ID id;
    DSL_IR_NODE_ID owner_node_id;
    UINT32 value_kind;
    UINT32 flags;
    STR_IDX name;
    STR_IDX value;
} DSL_IR_ATTRIBUTE_RECORD;

typedef enum {
    DSL_IR_VALUE_UNKNOWN = 0,
    DSL_IR_VALUE_OPERATOR_RESULT = 1,
    DSL_IR_VALUE_SYMBOL = 2,
    DSL_IR_VALUE_CONSTANT = 3
} DSL_IR_VALUE_KIND;

typedef struct {
    DSL_IR_VALUE_ID id;
    UINT32 value_kind;
    UINT32 tensor_descriptor_id;
    DSL_IR_NODE_ID producer_node_id;
    TY_IDX ty;
    ST_IDX st;
    UINT32 flags;
    UINT32 reserved;
    STR_IDX name;
    STR_IDX metadata;
} DSL_IR_VALUE_RECORD;

typedef enum {
    DSL_IR_VALUE_FLAG_NONE = 0,
    DSL_IR_VALUE_FLAG_REDIRECTED = 0x00000001,
    DSL_IR_VALUE_FLAG_LOWERED = 0x00000002,
    DSL_IR_VALUE_FLAG_DEAD_ELIDED = 0x00000004
} DSL_IR_VALUE_FLAG;

/*
 * Runtime-only borrowed view of one external tensor constant. The underlying
 * facts remain in the existing DSL value, ST metadata, and canonical TY
 * descriptor tables; this view does not add a mapped-image record. String
 * fields must not be retained across owner-PU table mutation or image reset.
 */
typedef struct {
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    TY_IDX descriptor_ty;
    ST_IDX st;
    TCON_IDX tensor_tcon;
    TY_IDX element_ty;
    INT32 rank;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
    const char *dtype;
    const char *logical_shape;
    const char *layout;
} DSL_IR_EXTERNAL_TENSOR_REFERENCE;

typedef enum {
    DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_ONLY = 0,
    DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO = 1
} DSL_IR_MATERIALIZE_SOURCE_POLICY;

/*
 * Runtime-only request for materializing converted side-file tensor values.
 * All requests are preflighted before any symbol, WN, or image table changes.
 * source_value_id must name an existing external tensor in the active PU. The
 * transaction derives the insertion BLOCK from PU_Info and insert_before. A
 * null call creates an entry-owned value without rewriting a call actual.
 */
typedef struct {
    const char *name;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    DSL_IR_VALUE_ID source_value_id;
    WN *insert_before;
    WN *call;
    UINT32 actual_ordinal;
    DSL_IR_VALUE_ID expected_actual_value_id;
    SRCPOS source_position;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
    UINT32 source_policy;
} DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT;

/* Runtime-only proof captured while a source PU's local symtab is active. */
typedef UINT64 DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE;

typedef struct {
    const char *name;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE source_handle;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    WN *insert_before;
    SRCPOS source_position;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
    const char *transformation_name;
    UINT32 transformation_version;
    UINT32 transformation_ordinal;
} DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    const char *transformation_name;
    UINT32 transformation_version;
    UINT32 transformation_ordinal;
} DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE;

extern BOOL DSL_IR_Capture_External_Tensor_Source
                                (PU_Info *source_pu_info,
                                 DSL_IR_VALUE_ID source_value_id,
                                 DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE
                                     *source_handle);
extern void DSL_IR_Typed_External_Tensor_Value_Request_Init
                                (DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST
                                     *request);
extern BOOL DSL_IR_Materialize_Typed_External_Tensor_Values
                                (PU_Info *pu_info,
                                 const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT
                                     *results);
extern BOOL DSL_IR_Image_Get_Typed_External_Tensor_Lineage
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE
                                     *lineage);
extern BOOL DSL_IR_Typed_External_Tensor_Validate_PU
                                (PU_Info *pu_info, FILE *diagnostic);

extern void DSL_IR_External_Tensor_Materialization_Request_Init
                                (DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST
                                     *request);

typedef struct {
    DSL_IR_VALUE_REFERENCE_ID id;
    DSL_IR_NODE_ID owner_node_id;
    UINT32 ordinal;
    DSL_IR_VALUE_ID value_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_IR_VALUE_REFERENCE_RECORD;

/*
 * Runtime-only request for replacing one logical operation while retaining
 * its node and result identities.  Referenced arrays are borrowed for the
 * duration of the call and are copied into the existing WHIRL image tables.
 */
typedef struct {
    DSL_IR_NODE_ID node_id;
    DSL_IR_OPCODE_DESCRIPTOR_ID opcode_descriptor_id;
    STR_IDX payload;
    const DSL_IR_VALUE_ID *operand_value_ids;
    UINT32 operand_count;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    UINT32 attribute_count;
    UINT32 result_value_kind;
} DSL_IR_NODE_REWRITE_REQUEST;

/*
 * Runtime-only request for atomically replacing a native DSL definition and
 * its logical image record. Operand templates and arrays are borrowed for the
 * call. Stable node, value, result symbol/type, and source identities remain
 * unchanged.
 */
typedef struct {
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 replacement_version;
    DSL_OPERATOR replacement_operator;
    const WN *const *operand_templates;
    const DSL_IR_VALUE_ID *operand_value_ids;
    UINT32 operand_count;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    UINT32 attribute_count;
    STR_IDX payload;
    UINT32 result_value_kind;
} DSL_IR_NATIVE_VALUE_REWRITE_REQUEST;

/*
 * Runtime-only redirect/retire request. PU_Info supplied to the transaction is
 * the sole tree/owner authority; the common containing BLOCK is derived from
 * the two definitions during preflight.
 */
typedef struct {
    WN *replacement_definition;
    DSL_IR_VALUE_ID replacement_value_id;
    WN *retiring_definition;
    DSL_IR_VALUE_ID retiring_value_id;
    DSL_OPERATOR expected_retiring_operator;
    UINT16 expected_retiring_version;
    UINT16 replacement_operand_ordinal;
} DSL_IR_NATIVE_VALUE_RETIRE_REQUEST;

/*
 * Runtime-only standard-WHIRL lowering transaction. The PU_Info supplied to
 * DSL_IR_Lower_Native_Values_To_Standard_Blocks is the sole owner/tree
 * authority; the native definition's containing BLOCK and a computed block's
 * final result STID are derived during preflight. Logical DSL node/value rows
 * remain immutable provenance and are marked LOWERED after their native
 * definition leaves the executable tree. No mapped-image row is added.
 */
typedef enum {
    DSL_IR_NATIVE_LOWER_UNKNOWN = 0,
    DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK = 1,
    DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION = 2,
    DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION = 3
} DSL_IR_NATIVE_VALUE_LOWER_MODE;

typedef enum {
    DSL_IR_LOWER_RELATION_UNKNOWN = 0,
    DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION = 1,
    DSL_IR_LOWER_RELATION_ROOT_PROMOTED_INPUT = 2,
    DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE = 3
} DSL_IR_LOWER_RELATION_KIND;

typedef struct {
    UINT32 relation_kind;
    DSL_RUNTIME_VALUE_PROJECTION_ID value_projection_id;
    DSL_RUNTIME_INPUT_ID runtime_input_id;
    DSL_RUNTIME_INPUT_BINDING_ID runtime_binding_id;
} DSL_IR_LOWER_RELATION;

typedef struct {
    WN *native_definition;
    DSL_IR_VALUE_ID source_value_id;
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 reserved;
    UINT32 mode;
    DSL_IR_LOWER_RELATION relation;
    WN *standard_block;
} DSL_IR_NATIVE_VALUE_LOWER_REQUEST;

typedef struct {
    DSL_IR_NODE_ID source_node_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 mode;
    UINT32 relation_kind;
    DSL_RUNTIME_VALUE_PROJECTION_ID value_projection_id;
    DSL_RUNTIME_INPUT_ID runtime_input_id;
    DSL_RUNTIME_INPUT_BINDING_ID runtime_binding_id;
    ST_IDX handle_st;
    TY_IDX handle_ty;
    UINT32 inserted_statement_count;
} DSL_IR_NATIVE_VALUE_LOWER_RESULT;

/*
 * Runtime-only, active-PU shape refinement request. The complete request
 * array is preflighted and committed atomically; no mapped-image layout is
 * added or changed.
 */
typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID value_id;
    TY_IDX expected_old_ty;
    TY_IDX refined_ty;
} DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST;

typedef struct {
    UINT32 request_count;
    UINT32 updated_st_count;
    UINT32 updated_wn_count;
    UINT32 updated_value_count;
    UINT32 rollback_count;
} DSL_IR_VALUE_TYPE_REFINEMENT_RESULT;

typedef enum {
    DSL_STATE_KIND_UNKNOWN = 0,
    DSL_STATE_KIND_RUNTIME_STATUS = 1,
    DSL_STATE_KIND_RANDOM = 2,
    DSL_STATE_KIND_MUTABLE_BUFFER = 3,
    DSL_STATE_KIND_COMMUNICATION = 4,
    DSL_STATE_KIND_OPAQUE = 5
} DSL_STATE_KIND;

typedef enum {
    DSL_STATE_OBJECT_FLAG_NONE = 0,
    DSL_STATE_OBJECT_UNIQUE_OWNERSHIP = 0x00000001
} DSL_STATE_OBJECT_FLAG;

typedef enum {
    DSL_STATE_EFFECT_UNKNOWN = 0,
    DSL_STATE_EFFECT_READ = 1,
    DSL_STATE_EFFECT_MODIFY = 2
} DSL_STATE_EFFECT_KIND;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 state_object_count;
    UINT32 state_effect_count;
    UINT32 flags;
    UINT32 reserved;
} DSL_EFFECT_IMAGE_HEADER;

typedef struct {
    DSL_STATE_OBJECT_ID id;
    UINT32 kind;
    ST_IDX owner_pu_st;
    ST_IDX st;
    STR_IDX name;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_STATE_OBJECT_RECORD;

typedef struct {
    DSL_STATE_EFFECT_ID id;
    DSL_IR_NODE_ID owner_node_id;
    DSL_STATE_OBJECT_ID state_object_id;
    UINT32 effect_kind;
    UINT32 ordinal;
    UINT32 flags;
} DSL_STATE_EFFECT_RECORD;

extern void DSL_IR_Image_Reset (void);
extern void DSL_IR_Image_Get_Header (DSL_IR_IMAGE_HEADER *header);
extern BOOL DSL_IR_Image_Has_Records (void);
extern BOOL DSL_IR_Image_Validate (FILE *diagnostic);
extern void DSL_IR_Image_Print (FILE *file);
extern BOOL DSL_IR_Image_Load_Mapped (const void *section_base,
                                      UINT64 section_size,
                                      FILE *diagnostic);

extern void DSL_Call_Image_Get_Header (DSL_CALL_IMAGE_HEADER *header);
extern void DSL_Call_Image_Reset (void);
extern BOOL DSL_Call_Image_Has_Records (void);
extern BOOL DSL_Call_Image_Validate (FILE *diagnostic);
extern BOOL DSL_Call_Image_Load_Mapped (const void *section_base,
                                        UINT64 section_size,
                                        FILE *diagnostic);
extern DSL_PU_SOURCE_IDENTITY_ID DSL_Call_Image_Add_PU_Identity
                                (const DSL_PU_SOURCE_IDENTITY_RECORD *record);
extern DSL_CALLSITE_METADATA_ID DSL_Call_Image_Add_Callsite
                                (ST_IDX owner_pu_st, WN *call,
                                 const DSL_CALLSITE_METADATA_RECORD *record);
extern BOOL DSL_Call_Image_Find_PU_Identity
                                (ST_IDX owner_pu_st,
                                 DSL_PU_SOURCE_IDENTITY_RECORD *record);
extern BOOL DSL_Call_Image_Find_Callsite
                                (const WN *call,
                                 DSL_CALLSITE_METADATA_RECORD *record);
extern const WN *DSL_Call_Image_Get_Call_WN
                                (DSL_CALLSITE_METADATA_ID id);
extern UINT32 DSL_Call_Image_PU_Identity_Count (void);
extern UINT32 DSL_Call_Image_Callsite_Count (void);
extern BOOL DSL_Call_Image_Get_PU_Identity
                                (DSL_PU_SOURCE_IDENTITY_ID id,
                                 DSL_PU_SOURCE_IDENTITY_RECORD *record);
extern BOOL DSL_Call_Image_Get_Callsite
                                (DSL_CALLSITE_METADATA_ID id,
                                 DSL_CALLSITE_METADATA_RECORD *record);
extern BOOL DSL_Call_Image_PU_Has_Calls (ST_IDX owner_pu_st);
extern BOOL DSL_Call_Image_Finalize_PU (ST_IDX owner_pu_st, WN_MAP off_map);
extern BOOL DSL_Call_Image_Load_PU (ST_IDX owner_pu_st,
                                    const void *tree_base,
                                    UINT64 tree_size);

extern void DSL_Call_ABI_Image_Get_Header (DSL_CALL_ABI_IMAGE_HEADER *header);
extern void DSL_Call_ABI_Image_Reset (void);
extern BOOL DSL_Call_ABI_Image_Has_Records (void);
extern BOOL DSL_Call_ABI_Image_Validate (FILE *diagnostic);
extern BOOL DSL_Call_ABI_Image_Load_Mapped (const void *section_base,
                                            UINT64 section_size,
                                            FILE *diagnostic);
extern DSL_CALL_ARGUMENT_ID DSL_Call_ABI_Image_Add_Argument
                                (const WN *call,
                                 const DSL_CALL_ARGUMENT_RECORD *record);
extern UINT32 DSL_Call_ABI_Image_Argument_Count (void);
extern BOOL DSL_Call_ABI_Image_Get_Argument
                                (DSL_CALL_ARGUMENT_ID id,
                                 DSL_CALL_ARGUMENT_RECORD *record);
extern BOOL DSL_Call_ABI_Image_Find_Argument
                                (const WN *call, UINT32 actual_ordinal,
                                 DSL_CALL_ARGUMENT_RECORD *record);
extern BOOL DSL_Call_ABI_Image_Find_Argument_By_Id
                                (DSL_CALLSITE_METADATA_ID callsite_id,
                                 UINT32 actual_ordinal,
                                 DSL_CALL_ARGUMENT_RECORD *record);
extern UINT32 DSL_Call_ABI_Image_Callee_Formal_Count
                                (ST_IDX callee_pu_st,
                                 UINT32 callee_formal_ordinal);
extern BOOL DSL_Call_ABI_Image_Get_Callee_Formal_Argument
                                (ST_IDX callee_pu_st,
                                 UINT32 callee_formal_ordinal,
                                 UINT32 index,
                                 DSL_CALL_ARGUMENT_RECORD *record);
extern BOOL DSL_Call_ABI_Image_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

extern void DSL_PU_Interface_Image_Get_Header
                                (DSL_PU_INTERFACE_IMAGE_HEADER *header);
extern void DSL_PU_Interface_Image_Reset (void);
extern BOOL DSL_PU_Interface_Image_Has_Records (void);
extern BOOL DSL_PU_Interface_Image_Validate (FILE *diagnostic);
extern BOOL DSL_PU_Interface_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
extern DSL_PU_FORMAL_ID DSL_PU_Interface_Image_Add_Formal
                                (const DSL_PU_FORMAL_RECORD *record);
extern UINT32 DSL_PU_Interface_Image_Formal_Count (void);
extern BOOL DSL_PU_Interface_Image_Get_Formal
                                (DSL_PU_FORMAL_ID id,
                                 DSL_PU_FORMAL_RECORD *record);
extern BOOL DSL_PU_Interface_Image_Find_Formal
                                (ST_IDX owner_pu_st,
                                 UINT32 formal_ordinal,
                                 DSL_PU_FORMAL_RECORD *record);
extern BOOL DSL_PU_Interface_Image_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

/*
 * Runtime-interface projection preserves source tensor rows and records the
 * standard-WHIRL handle ABI separately.  The driver owns PU selection; these
 * APIs never retain local WN or ST pointers across PU boundaries.
 */
extern void DSL_Runtime_Interface_Image_Get_Header
                                (DSL_RUNTIME_INTERFACE_IMAGE_HEADER *header);
extern void DSL_Runtime_Interface_Image_Reset (void);
extern BOOL DSL_Runtime_Interface_Image_Has_Records (void);
extern BOOL DSL_Runtime_Interface_Image_Validate (FILE *diagnostic);
extern BOOL DSL_Runtime_Interface_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
extern UINT32 DSL_Runtime_Interface_Image_Value_Count (void);
extern UINT32 DSL_Runtime_Interface_Image_Call_Count (void);
extern BOOL DSL_Runtime_Interface_Image_Get_Value
                                (DSL_RUNTIME_VALUE_PROJECTION_ID id,
                                 DSL_RUNTIME_VALUE_PROJECTION_RECORD *record);
extern BOOL DSL_Runtime_Interface_Image_Get_Call
                                (DSL_RUNTIME_CALL_PROJECTION_ID id,
                                 DSL_RUNTIME_CALL_PROJECTION_RECORD *record);
extern BOOL DSL_Runtime_Interface_Image_Find_Value
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID source_value_id,
                                 DSL_RUNTIME_VALUE_PROJECTION_RECORD *record);
extern BOOL DSL_Runtime_Interface_Image_Find_Call
                                (DSL_CALLSITE_METADATA_ID callsite_id,
                                 UINT32 actual_ordinal,
                                 DSL_RUNTIME_CALL_PROJECTION_RECORD *record);
extern BOOL DSL_Runtime_Interface_Plan_Validate
                                (const DSL_RUNTIME_INTERFACE_PLAN *plan,
                                 FILE *diagnostic);
extern void DSL_Runtime_Interface_Result_Init
                                (DSL_RUNTIME_INTERFACE_RESULT *result);
extern BOOL DSL_Runtime_Interface_Apply_PU
                                (PU_Info *pu,
                                 const DSL_RUNTIME_INTERFACE_PLAN *plan,
                                 FILE *diagnostic,
                                 DSL_RUNTIME_INTERFACE_RESULT *result);
extern BOOL DSL_Runtime_Interface_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

/*
 * Program-interface evolution is committed only through this combined
 * transaction.  The plan uses global identities; local ST/WN interpretation
 * occurs only while the owning PU is active.
 */
extern BOOL DSL_Program_Interface_Plan_Validate
                                (const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
                                 const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
                                 FILE *diagnostic);
extern void DSL_Program_Interface_Result_Init
                                (DSL_PROGRAM_INTERFACE_RESULT *result);
extern BOOL DSL_Program_Interface_Apply_PU
                                (PU_Info *pu,
                                 const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
                                 const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
                                 FILE *diagnostic,
                                 DSL_PROGRAM_INTERFACE_RESULT *result);
extern BOOL DSL_Program_Interface_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);
extern BOOL DSL_Program_Interface_Validate_Lowered_PU
                                (PU_Info *pu, FILE *diagnostic);

/*
 * Program-interface evolution preserves canonical v1 provenance while
 * recording effective ABI retirement and explicit runtime-input flow.
 * Mutation is available only through the combined transaction service.
 */
extern void DSL_Program_Interface_Image_Get_Header
                                (DSL_PROGRAM_INTERFACE_IMAGE_HEADER *header);
extern void DSL_Program_Interface_Image_Reset (void);
extern BOOL DSL_Program_Interface_Image_Has_Records (void);
extern BOOL DSL_Program_Interface_Image_Validate (FILE *diagnostic);
extern BOOL DSL_Program_Interface_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
extern BOOL DSL_Program_Runtime_Interface_Images_Load_Mapped
                                (const void *program_section_base,
                                 UINT64 program_section_size,
                                 const void *runtime_section_base,
                                 UINT64 runtime_section_size,
                                 FILE *diagnostic);
extern UINT32 DSL_Program_Interface_Image_Retired_Formal_Count (void);
extern UINT32 DSL_Program_Interface_Image_Retired_Call_Count (void);
extern UINT32 DSL_Program_Interface_Image_Runtime_Input_Count (void);
extern UINT32 DSL_Program_Interface_Image_Runtime_Binding_Count (void);
extern UINT32 DSL_Program_Interface_Image_Runtime_Call_Count (void);
extern BOOL DSL_Program_Interface_Image_Get_Retired_Formal
                                (DSL_RETIRED_FORMAL_ID id,
                                 DSL_RETIRED_FORMAL_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Get_Retired_Call
                                (DSL_RETIRED_CALL_ARGUMENT_ID id,
                                 DSL_RETIRED_CALL_ARGUMENT_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Get_Runtime_Input
                                (DSL_RUNTIME_INPUT_ID id,
                                 DSL_RUNTIME_INPUT_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Get_Runtime_Binding
                                (DSL_RUNTIME_INPUT_BINDING_ID id,
                                 DSL_RUNTIME_INPUT_BINDING_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Get_Runtime_Call
                                (DSL_RUNTIME_INPUT_CALL_ID id,
                                 DSL_RUNTIME_INPUT_CALL_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Find_Retired_Formal
                                (DSL_PU_FORMAL_ID pu_formal_id,
                                 DSL_RETIRED_FORMAL_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Find_Retired_Call
                                (DSL_CALL_ARGUMENT_ID call_argument_id,
                                 DSL_RETIRED_CALL_ARGUMENT_RECORD *record);
extern BOOL DSL_Program_Interface_Image_Find_Runtime_Binding
                                (ST_IDX owner_pu_st,
                                 UINT32 final_formal_ordinal,
                                 DSL_RUNTIME_INPUT_BINDING_RECORD *record);

extern void DSL_Effect_Image_Get_Header (DSL_EFFECT_IMAGE_HEADER *header);
extern void DSL_Effect_Image_Reset (void);
extern BOOL DSL_Effect_Image_Has_Records (void);
extern BOOL DSL_Effect_Image_Validate (FILE *diagnostic);
extern BOOL DSL_Effect_Image_Load_Mapped (const void *section_base,
                                          UINT64 section_size,
                                          FILE *diagnostic);
extern void DSL_State_Object_Record_Init (DSL_STATE_OBJECT_RECORD *record);
extern void DSL_State_Effect_Record_Init (DSL_STATE_EFFECT_RECORD *record);
extern DSL_STATE_OBJECT_ID DSL_Effect_Image_Add_State_Object
                                (const DSL_STATE_OBJECT_RECORD *record);
extern DSL_STATE_EFFECT_ID DSL_Effect_Image_Add_State_Effect
                                (const DSL_STATE_EFFECT_RECORD *record);
extern UINT32 DSL_Effect_Image_State_Object_Count (void);
extern UINT32 DSL_Effect_Image_State_Effect_Count (void);
extern BOOL DSL_Effect_Image_Get_State_Object
                                (DSL_STATE_OBJECT_ID id,
                                 DSL_STATE_OBJECT_RECORD *record);
extern BOOL DSL_Effect_Image_Get_State_Effect
                                (DSL_STATE_EFFECT_ID id,
                                 DSL_STATE_EFFECT_RECORD *record);
extern const char *DSL_State_Kind_Name (DSL_STATE_KIND kind);
extern const char *DSL_State_Effect_Kind_Name
                                (DSL_STATE_EFFECT_KIND effect_kind);

extern void DSL_IR_Opcode_Descriptor_Record_Init
                                (DSL_IR_OPCODE_DESCRIPTOR_RECORD *record);
extern void DSL_IR_Node_Record_Init (DSL_IR_NODE_RECORD *record);
extern void DSL_IR_Attribute_Record_Init (DSL_IR_ATTRIBUTE_RECORD *record);
extern void DSL_IR_Value_Record_Init (DSL_IR_VALUE_RECORD *record);
extern void DSL_IR_Value_Reference_Record_Init
                                (DSL_IR_VALUE_REFERENCE_RECORD *record);

extern DSL_IR_OPCODE_DESCRIPTOR_ID DSL_IR_Image_Add_Opcode_Descriptor
                                (const DSL_IR_OPCODE_DESCRIPTOR_RECORD *record);
extern DSL_IR_OPCODE_DESCRIPTOR_ID DSL_IR_Image_Find_Opcode_Descriptor
                                (UINT32 logical_operator,
                                 UINT32 version);
extern DSL_IR_OPCODE_DESCRIPTOR_ID DSL_IR_Image_Ensure_Opcode_Descriptor
                                (UINT32 logical_operator,
                                 UINT32 version);
extern DSL_IR_NODE_ID DSL_IR_Image_Add_Node
                                (const DSL_IR_NODE_RECORD *record);
extern DSL_IR_ATTRIBUTE_ID DSL_IR_Image_Add_Attribute
                                (const DSL_IR_ATTRIBUTE_RECORD *record);
extern DSL_IR_VALUE_ID DSL_IR_Image_Add_Value
                                (const DSL_IR_VALUE_RECORD *record);
extern DSL_IR_VALUE_REFERENCE_ID DSL_IR_Image_Add_Value_Reference
                                (const DSL_IR_VALUE_REFERENCE_RECORD *record);
extern BOOL DSL_IR_Image_Set_Node_Links
                                (DSL_IR_NODE_ID node_id,
                                 DSL_IR_VALUE_REFERENCE_ID first_operand_id,
                                 UINT32 operand_count,
                                 DSL_IR_ATTRIBUTE_ID first_attribute_id,
                                 UINT32 attribute_count,
                                 DSL_IR_VALUE_ID result_value_id);
extern BOOL DSL_IR_Image_Rewrite_Node
                                (const DSL_IR_NODE_REWRITE_REQUEST *request);
/*
 * Resolve one native STID definition to its stable logical result value in the
 * exact active PU. PU_Info is the sole owner authority; the query rejects an
 * inactive PU, local-ST collisions, and physical/logical opcode disagreement.
 * It borrows no state and performs no mutation.
 */
extern BOOL DSL_IR_Image_Find_Definition_Value
                                (PU_Info *pu_info,
                                 const WN *definition,
                                 DSL_IR_VALUE_RECORD *value_record);
extern BOOL DSL_IR_Image_Get_External_Tensor_Reference
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference);
/*
 * Preflight and atomically materialize a complete converted-tensor request
 * array in one active PU. Parent BLOCKs are derived from PU_Info.
 */
extern BOOL DSL_IR_Materialize_External_Tensor_Values
                                (PU_Info *pu_info,
                                 const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT
                                     *results);
/*
 * Atomically replace one native/logical operation in the exact active PU.
 * Preflight proves the expected operator, schema, operands, result ST/TY,
 * owner, and source identity. Commit preserves stable node/value identity and
 * updates the physical WN plus logical image together; rejection leaves both
 * unchanged.
 */
extern BOOL DSL_IR_Rewrite_Native_Value
                                (PU_Info *pu_info,
                                 WN *definition,
                                 DSL_IR_VALUE_ID value_id,
                                 const DSL_IR_NATIVE_VALUE_REWRITE_REQUEST
                                     *request);
/*
 * Atomically redirect owner-safe uses to a dominating replacement and retire
 * one pure definition. The transaction derives the PU tree and common parent
 * BLOCK from PU_Info, preflights WHIRL, image, call-ABI, effect, and REGION
 * uses, then commits without accepting caller-supplied physical ownership.
 */
extern BOOL DSL_IR_Redirect_And_Retire_Native_Value
                                (PU_Info *pu_info,
                                 const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST
                                     *request);
/*
 * Preflight and atomically lower a complete request set for one active PU.
 * The API derives PU ownership, the native definitions' containing blocks,
 * and each computed standard block's final result STID from its inputs. It
 * mutates WHIRL and logical lowered flags only after all requests, relations,
 * source uses, effects, and detached block contracts validate; failure before
 * commit leaves the physical tree and image unchanged.
 */
extern BOOL DSL_IR_Lower_Native_Values_To_Standard_Blocks
                                (PU_Info *pu_info,
                                 const DSL_IR_NATIVE_VALUE_LOWER_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 FILE *diagnostic,
                                 DSL_IR_NATIVE_VALUE_LOWER_RESULT *results);
extern BOOL DSL_IR_Image_Resolve_Lowered_Relation
                                (DSL_IR_VALUE_ID value_id,
                                 DSL_IR_NATIVE_VALUE_LOWER_RESULT *result);
extern BOOL DSL_IR_Image_Validate_Lowered_Relations (FILE *diagnostic);
extern BOOL DSL_IR_Refine_Native_Value_Types
                                (PU_Info *pu_info,
                                 const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 FILE *diagnostic,
                                 DSL_IR_VALUE_TYPE_REFINEMENT_RESULT *result);
extern BOOL DSL_IR_Image_Value_Redirect_Target
                                (DSL_IR_VALUE_ID value_id,
                                 DSL_IR_VALUE_ID *target_value_id);
extern BOOL DSL_IR_Image_Find_Value
                                (ST_IDX st,
                                 const char *name,
                                 DSL_IR_VALUE_RECORD *record);
extern BOOL DSL_IR_Image_Find_PU_Value
                                (ST_IDX st,
                                 const char *name,
                                 const char *owner_pu,
                                 DSL_IR_VALUE_RECORD *record);

extern UINT32 DSL_IR_Image_Opcode_Descriptor_Count (void);
extern UINT32 DSL_IR_Image_Node_Count (void);
extern UINT32 DSL_IR_Image_Executable_Node_Count (void);
extern UINT32 DSL_IR_Image_Attribute_Count (void);
extern UINT32 DSL_IR_Image_Value_Count (void);
extern UINT32 DSL_IR_Image_Value_Reference_Count (void);

extern BOOL DSL_IR_Image_Get_Opcode_Descriptor
                                (DSL_IR_OPCODE_DESCRIPTOR_ID id,
                                 DSL_IR_OPCODE_DESCRIPTOR_RECORD *record);
extern BOOL DSL_IR_Image_Get_Node (DSL_IR_NODE_ID id,
                                   DSL_IR_NODE_RECORD *record);
extern BOOL DSL_IR_Image_Get_Attribute
                                (DSL_IR_ATTRIBUTE_ID id,
                                 DSL_IR_ATTRIBUTE_RECORD *record);
extern BOOL DSL_IR_Image_Get_Value (DSL_IR_VALUE_ID id,
                                    DSL_IR_VALUE_RECORD *record);
extern BOOL DSL_IR_Image_Get_Value_Reference
                                (DSL_IR_VALUE_REFERENCE_ID id,
                                 DSL_IR_VALUE_REFERENCE_RECORD *record);

#endif /* dsl_ir_image_INCLUDED */
