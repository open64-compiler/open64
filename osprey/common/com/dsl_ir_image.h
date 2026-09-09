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

#define DSL_STATE_OBJECT_INVALID_ID 0
#define DSL_STATE_EFFECT_INVALID_ID 0
#define DSL_PU_SOURCE_IDENTITY_INVALID_ID 0
#define DSL_CALLSITE_METADATA_INVALID_ID 0

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

/*
 * Runtime-only request for materializing converted side-file tensor values.
 * All requests are preflighted before any symbol, WN, or image table changes.
 * source_value_id must name an existing external tensor in the owner PU. A
 * null call creates an entry-owned value without rewriting a call actual.
 */
typedef struct {
    const char *name;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    DSL_IR_VALUE_ID source_value_id;
    WN *insertion_block;
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
} DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT;

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
extern BOOL DSL_IR_Image_Find_Definition_Value
                                (ST_IDX owner_pu_st,
                                 const WN *definition,
                                 DSL_IR_VALUE_RECORD *value_record);
extern BOOL DSL_IR_Image_Get_External_Tensor_Reference
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference);
extern BOOL DSL_IR_Materialize_External_Tensor_Values
                                (ST_IDX owner_pu_st,
                                 const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT
                                     *results);
extern BOOL DSL_IR_Rewrite_Native_Value
                                (ST_IDX owner_pu_st,
                                 WN *definition,
                                 DSL_IR_VALUE_ID value_id,
                                 const DSL_IR_NATIVE_VALUE_REWRITE_REQUEST
                                     *request);
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
