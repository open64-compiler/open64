/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_ir_image_INCLUDED
#define dsl_ir_image_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "symtab_idx.h"

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

#define DSL_STATE_OBJECT_INVALID_ID 0
#define DSL_STATE_EFFECT_INVALID_ID 0

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
