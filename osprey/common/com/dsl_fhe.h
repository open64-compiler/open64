/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_fhe_INCLUDED
#define dsl_fhe_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_builder.h"
#include "dsl_ir_image.h"
#include "symtab_idx.h"

#define DSL_FHE_IMAGE_MAGIC   0x46484531
#define DSL_FHE_IMAGE_VERSION 1

#define DSL_FHE_IMAGE_HEADER_SIZE                 64
#define DSL_FHE_CONFIG_RECORD_SIZE                64
#define DSL_FHE_ENTRY_CONTRACT_RECORD_SIZE        48
#define DSL_FHE_ENTRY_VALUE_RECORD_SIZE           32
#define DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD_SIZE 56
#define DSL_FHE_TENSOR_BINDING_RECORD_SIZE        24
#define DSL_FHE_KEY_REQUIREMENT_RECORD_SIZE       48

typedef UINT32 DSL_FHE_CONFIG_ID;
typedef UINT32 DSL_FHE_ENTRY_CONTRACT_ID;
typedef UINT32 DSL_FHE_ENTRY_VALUE_ID;
typedef UINT32 DSL_FHE_ENCRYPTION_DESCRIPTOR_ID;
typedef UINT32 DSL_FHE_TENSOR_BINDING_ID;
typedef UINT32 DSL_FHE_KEY_REQUIREMENT_ID;
typedef UINT32 DSL_FHE_CKKS_VALUE_STATE_ID;

#define DSL_FHE_CONFIG_INVALID_ID                0
#define DSL_FHE_ENTRY_CONTRACT_INVALID_ID        0
#define DSL_FHE_ENTRY_VALUE_INVALID_ID           0
#define DSL_FHE_ENCRYPTION_DESCRIPTOR_INVALID_ID 0
#define DSL_FHE_TENSOR_BINDING_INVALID_ID        0
#define DSL_FHE_KEY_REQUIREMENT_INVALID_ID       0
#define DSL_FHE_CKKS_VALUE_STATE_INVALID_ID      0

typedef enum {
    DSL_FHE_IMAGE_RECORD_UNKNOWN = 0,
    DSL_FHE_IMAGE_RECORD_CONFIG = 1,
    DSL_FHE_IMAGE_RECORD_ENTRY_CONTRACT = 2,
    DSL_FHE_IMAGE_RECORD_ENTRY_VALUE = 3,
    DSL_FHE_IMAGE_RECORD_ENCRYPTION_DESCRIPTOR = 4,
    DSL_FHE_IMAGE_RECORD_TENSOR_BINDING = 5,
    DSL_FHE_IMAGE_RECORD_KEY_REQUIREMENT = 6
} DSL_FHE_IMAGE_RECORD_KIND;

typedef enum {
    DSL_FHE_IMAGE_CAP_CONFIG = 0x00000001,
    DSL_FHE_IMAGE_CAP_ENTRY_CONTRACT = 0x00000002,
    DSL_FHE_IMAGE_CAP_ENTRY_VALUE = 0x00000004,
    DSL_FHE_IMAGE_CAP_ENCRYPTION_DESCRIPTOR = 0x00000008,
    DSL_FHE_IMAGE_CAP_TENSOR_BINDING = 0x00000010,
    DSL_FHE_IMAGE_CAP_KEY_REQUIREMENT = 0x00000020
} DSL_FHE_IMAGE_CAPABILITY;

typedef enum {
    DSL_FHE_SCHEME_UNKNOWN = 0,
    DSL_FHE_SCHEME_CKKS = 1
} DSL_FHE_SCHEME;

typedef enum {
    DSL_FHE_SECURITY_UNKNOWN = 0,
    DSL_FHE_SECURITY_128_CLASSIC = 1
} DSL_FHE_SECURITY_LEVEL;

typedef enum {
    DSL_FHE_POLICY_UNKNOWN = 0,
    DSL_FHE_POLICY_AUTO = 1,
    DSL_FHE_POLICY_EXPLICIT = 2,
    DSL_FHE_POLICY_INHERIT = 3
} DSL_FHE_POLICY;

typedef enum {
    DSL_FHE_BOOTSTRAP_UNKNOWN = 0,
    DSL_FHE_BOOTSTRAP_AUTO = 1,
    DSL_FHE_BOOTSTRAP_ON = 2,
    DSL_FHE_BOOTSTRAP_MANUAL = 3,
    DSL_FHE_BOOTSTRAP_OFF = 4
} DSL_FHE_BOOTSTRAP_POLICY;

typedef enum {
    DSL_FHE_BACKEND_UNKNOWN = 0,
    DSL_FHE_BACKEND_AUTO = 1,
    DSL_FHE_BACKEND_OPENFHE = 2,
    DSL_FHE_BACKEND_MOCK = 3
} DSL_FHE_BACKEND_POLICY;

typedef enum {
    DSL_FHE_VALUE_CLASS_UNKNOWN = 0,
    DSL_FHE_VALUE_CLASS_CIPHERTEXT = 1,
    DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT = 2,
    DSL_FHE_VALUE_CLASS_CLEAR = 3
} DSL_FHE_VALUE_CLASS;

typedef enum {
    DSL_FHE_ENTRY_VALUE_UNKNOWN = 0,
    DSL_FHE_ENTRY_VALUE_INPUT = 1,
    DSL_FHE_ENTRY_VALUE_OUTPUT = 2,
    DSL_FHE_ENTRY_VALUE_PARAMETER = 3
} DSL_FHE_ENTRY_VALUE_ROLE;

typedef enum {
    DSL_FHE_PARAMETER_POLICY_UNKNOWN = 0,
    DSL_FHE_PARAMETER_POLICY_PLAINTEXT = 1,
    DSL_FHE_PARAMETER_POLICY_ENCODED_PLAINTEXT = 2
} DSL_FHE_PARAMETER_POLICY;

typedef enum {
    DSL_FHE_ENCODING_UNKNOWN = 0,
    DSL_FHE_ENCODING_NONE = 1,
    DSL_FHE_ENCODING_CKKS_PACKED = 2
} DSL_FHE_ENCODING_POLICY;

typedef enum {
    DSL_FHE_PACKING_UNKNOWN = 0,
    DSL_FHE_PACKING_AUTO = 1,
    DSL_FHE_PACKING_METAKERNEL = 2,
    DSL_FHE_PACKING_FHELIPE = 3,
    DSL_FHE_PACKING_INHERIT = 4
} DSL_FHE_PACKING_POLICY;

typedef enum {
    DSL_FHE_KEY_UNKNOWN = 0,
    DSL_FHE_KEY_PUBLIC = 1,
    DSL_FHE_KEY_RELINEARIZATION = 2,
    DSL_FHE_KEY_ROTATION = 3,
    DSL_FHE_KEY_BOOTSTRAP = 4
} DSL_FHE_KEY_CLASS;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_kind_count;
    UINT32 capabilities;
    UINT32 flags;
    UINT32 config_count;
    UINT32 entry_contract_count;
    UINT32 entry_value_count;
    UINT32 encryption_descriptor_count;
    UINT32 tensor_binding_count;
    UINT32 key_requirement_count;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
    UINT32 reserved3;
} DSL_FHE_IMAGE_HEADER;

typedef struct {
    DSL_FHE_CONFIG_ID id;
    UINT32 flags;
    UINT32 provenance_mask;
    UINT32 scheme;
    UINT32 security_level;
    UINT32 ring_dimension;
    UINT32 multiplicative_depth_policy;
    UINT32 multiplicative_depth;
    UINT32 scale_bits;
    UINT32 first_modulus_bits;
    UINT32 slot_count_policy;
    UINT32 slot_count;
    UINT32 key_switch_policy;
    UINT32 bootstrap_policy;
    UINT32 backend_policy;
    UINT32 reserved;
} DSL_FHE_COMPILATION_CONFIG_RECORD;

typedef struct {
    DSL_FHE_ENTRY_CONTRACT_ID id;
    ST_IDX owner_pu_st;
    DSL_FHE_CONFIG_ID config_id;
    DSL_FHE_ENTRY_VALUE_ID first_entry_value_id;
    UINT32 entry_value_count;
    UINT32 input_count;
    UINT32 output_count;
    UINT32 parameter_count;
    UINT32 encrypted_io_policy;
    UINT32 parameter_policy;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_ENTRY_CONTRACT_RECORD;

typedef struct {
    DSL_FHE_ENTRY_VALUE_ID id;
    DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id;
    DSL_IR_VALUE_ID value_id;
    UINT32 ordinal;
    UINT32 role;
    UINT32 value_class;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 flags;
} DSL_FHE_ENTRY_VALUE_RECORD;

typedef struct {
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID id;
    UINT32 value_class;
    UINT32 scheme;
    DSL_FHE_CONFIG_ID config_id;
    STR_IDX key_set_name;
    UINT32 slot_count_policy;
    UINT32 slot_count;
    UINT32 encoding_policy;
    UINT32 packing_policy;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD;

typedef struct {
    DSL_FHE_TENSOR_BINDING_ID id;
    TY_IDX tensor_ty;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_FHE_TENSOR_BINDING_RECORD;

typedef struct {
    DSL_FHE_KEY_REQUIREMENT_ID id;
    DSL_FHE_CONFIG_ID config_id;
    STR_IDX key_set_name;
    UINT32 key_class;
    INT32 rotation_offset;
    STR_IDX bootstrap_profile;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_FHE_KEY_REQUIREMENT_RECORD;

typedef struct {
    DSL_FHE_CONFIG_ID config_id;
    UINT32 input_count;
    UINT32 output_count;
    UINT32 parameter_count;
    UINT32 encrypted_io_policy;
    UINT32 parameter_policy;
    UINT32 flags;
} DSL_FHE_ENTRY_CONTRACT_INFO;

typedef struct {
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 value_class;
    UINT32 flags;
} DSL_FHE_ENTRY_VALUE_INFO;

extern void DSL_FHE_Image_Reset (void);
extern void DSL_FHE_Image_Get_Header (DSL_FHE_IMAGE_HEADER *header);
extern BOOL DSL_FHE_Image_Has_Records (void);
extern BOOL DSL_FHE_Image_Validate (FILE *diagnostic);
extern BOOL DSL_FHE_Image_Load_Mapped (const void *section_base,
                                       UINT64 section_size,
                                       FILE *diagnostic);
extern void DSL_FHE_Image_Print (FILE *file);

extern void DSL_FHE_Compilation_Config_Record_Init
                                (DSL_FHE_COMPILATION_CONFIG_RECORD *record);
extern void DSL_FHE_Entry_Contract_Record_Init
                                (DSL_FHE_ENTRY_CONTRACT_RECORD *record);
extern void DSL_FHE_Entry_Value_Record_Init
                                (DSL_FHE_ENTRY_VALUE_RECORD *record);
extern void DSL_FHE_Encryption_Descriptor_Record_Init
                                (DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);
extern void DSL_FHE_Tensor_Binding_Record_Init
                                (DSL_FHE_TENSOR_BINDING_RECORD *record);
extern void DSL_FHE_Key_Requirement_Record_Init
                                (DSL_FHE_KEY_REQUIREMENT_RECORD *record);

extern DSL_FHE_CONFIG_ID DSL_FHE_Intern_Compilation_Config
                         (const DSL_FHE_COMPILATION_CONFIG_RECORD *record);
extern DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
    DSL_FHE_Intern_Encryption_Descriptor
                         (const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);
extern DSL_FHE_TENSOR_BINDING_ID DSL_FHE_Intern_Tensor_Binding
                         (TY_IDX tensor_ty,
                          DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
                          UINT32 flags);
extern DSL_FHE_ENTRY_CONTRACT_ID DSL_FHE_Add_Entry_Contract
                         (const DSL_FHE_ENTRY_CONTRACT_RECORD *record);
extern DSL_FHE_ENTRY_VALUE_ID DSL_FHE_Add_Entry_Value
                         (const DSL_FHE_ENTRY_VALUE_RECORD *record);
extern BOOL DSL_FHE_Set_Entry_Value_Range
                         (DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
                          DSL_FHE_ENTRY_VALUE_ID first_entry_value_id,
                          UINT32 entry_value_count);
extern DSL_FHE_KEY_REQUIREMENT_ID DSL_FHE_Intern_Key_Requirement
                         (const DSL_FHE_KEY_REQUIREMENT_RECORD *record);

extern UINT32 DSL_FHE_Config_Count (void);
extern UINT32 DSL_FHE_Entry_Contract_Count (void);
extern UINT32 DSL_FHE_Entry_Value_Count (void);
extern UINT32 DSL_FHE_Encryption_Descriptor_Count (void);
extern UINT32 DSL_FHE_Tensor_Binding_Count (void);
extern UINT32 DSL_FHE_Key_Requirement_Count (void);

extern BOOL DSL_FHE_Get_Compilation_Config
                         (DSL_FHE_CONFIG_ID id,
                          DSL_FHE_COMPILATION_CONFIG_RECORD *record);
extern BOOL DSL_FHE_Get_Entry_Contract
                         (DSL_FHE_ENTRY_CONTRACT_ID id,
                          DSL_FHE_ENTRY_CONTRACT_RECORD *record);
extern BOOL DSL_FHE_Get_Entry_Value
                         (DSL_FHE_ENTRY_VALUE_ID id,
                          DSL_FHE_ENTRY_VALUE_RECORD *record);
extern BOOL DSL_FHE_Get_Encryption_Descriptor
                         (DSL_FHE_ENCRYPTION_DESCRIPTOR_ID id,
                          DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);
extern BOOL DSL_FHE_Get_Tensor_Binding
                         (DSL_FHE_TENSOR_BINDING_ID id,
                          DSL_FHE_TENSOR_BINDING_RECORD *record);
extern BOOL DSL_FHE_Find_Tensor_Binding
                         (TY_IDX tensor_ty,
                          DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
                          DSL_FHE_TENSOR_BINDING_RECORD *record);
extern BOOL DSL_FHE_Get_Key_Requirement
                         (DSL_FHE_KEY_REQUIREMENT_ID id,
                          DSL_FHE_KEY_REQUIREMENT_RECORD *record);

extern DSL_FHE_TENSOR_BINDING_ID
    DSL_Builder_Bind_FHE_Tensor_Descriptor
                         (TY_IDX tensor_ty,
                          DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
                          UINT32 flags);
extern DSL_FHE_ENTRY_CONTRACT_ID
    DSL_Builder_Attach_FHE_Entry_Contract
                         (DSL_BUILDER_PROGRAM_UNIT pu,
                          const DSL_FHE_ENTRY_CONTRACT_INFO *info);
extern DSL_FHE_ENTRY_VALUE_ID DSL_Builder_Declare_FHE_Entry_Value
                         (DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
                          DSL_BUILDER_VALUE value,
                          UINT32 ordinal,
                          DSL_FHE_ENTRY_VALUE_ROLE role,
                          const DSL_FHE_ENTRY_VALUE_INFO *info);
extern BOOL DSL_Builder_Get_FHE_Value_Encryption_Descriptor
                         (DSL_BUILDER_VALUE value,
                          DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);

#endif /* dsl_fhe_INCLUDED */
