/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Persistent DSL mapped-image schema only. The seven ELF section families
 * below retain their existing headers, fixed rows, IDs, flags and versions.
 * Managed storage, borrowed views and compiler transactions are declared
 * by their owning services, never re-exported from this schema.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef dsl_ir_image_INCLUDED
#define dsl_ir_image_INCLUDED

#include "defs.h"
#include "dsl_opcode.h"
#include "srcpos.h"
#include "symtab_idx.h"
#include "targ_const.h"

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

/* Reserved all-ones sentinel, not a valid zero-based actual/formal ordinal. */
#define DSL_CALL_ARGUMENT_INVALID_ORDINAL ((UINT32)-1)

#define DSL_PU_FORMAL_INVALID_ID 0

/* Unassigned source-formal ordinal; ordinal zero denotes the first formal. */
#define DSL_PU_FORMAL_INVALID_ORDINAL ((UINT32)-1)

#define DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID 0

#define DSL_RUNTIME_CALL_PROJECTION_INVALID_ID 0

/* Local-value projections have no formal ordinal and use this sentinel;
 * formal bindings and call actual/formal ordinals must use real positions. */
#define DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ((UINT32)-1)

#define DSL_RETIRED_FORMAL_INVALID_ID 0

#define DSL_RETIRED_CALL_ARGUMENT_INVALID_ID 0

#define DSL_RUNTIME_INPUT_INVALID_ID 0

#define DSL_RUNTIME_INPUT_BINDING_INVALID_ID 0

#define DSL_RUNTIME_INPUT_CALL_INVALID_ID 0

/* Persistent .WHIRL.dsl_calls header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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

/* Persistent .WHIRL.dsl_call_abi header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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

/* Persistent .WHIRL.dsl_pu_interface header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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

/* Persistent .WHIRL.dsl_runtime_interface header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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

typedef enum {
    DSL_RUNTIME_VALUE_PROJECTION_FLAG_NONE = 0,
    DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW = 0x00000001
} DSL_RUNTIME_VALUE_PROJECTION_FLAG;

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

/* Persistent .WHIRL.dsl_program_interface header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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

/* Persistent .WHIRL.dsl header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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
    DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW = 0x00000008,
    DSL_IR_NODE_FLAG_GENERATED_EXTERNAL = 0x00000010,
    DSL_IR_NODE_FLAG_RETIRED_VIEW = 0x00000020
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

typedef struct {
    DSL_IR_VALUE_REFERENCE_ID id;
    DSL_IR_NODE_ID owner_node_id;
    UINT32 ordinal;
    DSL_IR_VALUE_ID value_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_IR_VALUE_REFERENCE_RECORD;

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

/* Persistent .WHIRL.dsl_effects header and fixed rows; ir_bwrite.cxx finalizes
 * the ELF section and ir_bread.cxx maps it through the image owner. */
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

#endif /* dsl_ir_image_INCLUDED */
