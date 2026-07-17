/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef open64_dsc_native_bridge_INCLUDED
#define open64_dsc_native_bridge_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long long Open64_DSC_Handle;

typedef struct {
    const char *name;
    const char *value;
} Open64_DSC_Attribute;

typedef struct {
    const char *kind;
    const char *dtype;
    int rank;
    const char *logical_shape;
    const char *traits;
    const char *layout;
    const char *sharding;
    const char *placement;
    const char *memory;
    const char *quantization;
    const char *runtime_state;
    const char *lineage;
} Open64_DSC_Tensor_Descriptor;

typedef struct {
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    unsigned long long byte_offset;
    unsigned long long byte_length;
    const char *checksum;
} Open64_DSC_External_Tensor_Reference;

typedef struct {
    unsigned int file_id;
    int line;
    unsigned short column;
    unsigned char statement_begin;
    unsigned char basic_block_begin;
} Open64_DSC_Source_Position;

typedef struct {
    unsigned int native_node_count;
    unsigned int result_symbol_count;
    unsigned int error_count;
    char *diagnostic;
    unsigned int diagnostic_capacity;
} Open64_DSC_Verify_Result;

typedef struct {
    const char *opcode_name;
    unsigned int opcode_name_len;
    unsigned int version;
    const char *payload;
} Open64_DSC_Marker_Info;

typedef Open64_DSC_Marker_Info Open64_DSC_Value_Info;

extern Open64_DSC_Handle Open64_DSC_Create_Tensor_Type
                                (const char *name,
                                 const char *dtype,
                                 int rank,
                                 const char *logical_shape);
extern Open64_DSC_Handle Open64_DSC_Intern_Tensor_Type
                                (const char *name,
                                 const Open64_DSC_Tensor_Descriptor *descriptor);
extern int Open64_DSC_Attach_Tensor_Descriptor
                                (Open64_DSC_Handle tensor_type,
                                 const Open64_DSC_Tensor_Descriptor *descriptor);
extern Open64_DSC_Handle Open64_DSC_Create_Tensor_Constant
                                (const char *name,
                                 const char *dtype,
                                 unsigned int rank,
                                 const char *logical_shape,
                                 const char *value_kind,
                                 const char *value);
extern Open64_DSC_Handle Open64_DSC_Create_Model_Input
                                (const char *name,
                                 Open64_DSC_Handle tensor_type,
                                 unsigned int input_ordinal);
extern Open64_DSC_Handle Open64_DSC_Create_External_Tensor_Constant
                                (const char *name,
                                 Open64_DSC_Handle tensor_type,
                                 const Open64_DSC_External_Tensor_Reference
                                     *reference);
extern Open64_DSC_Handle Open64_DSC_Create_Operator
                                (const char *opcode_name,
                                 unsigned int version,
                                 const Open64_DSC_Handle *kids,
                                 unsigned int kid_count,
                                 const Open64_DSC_Attribute *attrs,
                                 unsigned int attr_count);
extern Open64_DSC_Handle Open64_DSC_Create_Operator_With_Result
                                (const char *opcode_name,
                                 unsigned int version,
                                 const Open64_DSC_Handle *kids,
                                 unsigned int kid_count,
                                 const Open64_DSC_Attribute *attrs,
                                 unsigned int attr_count,
                                 const char *result_name,
                                 Open64_DSC_Handle result_type);
extern Open64_DSC_Handle Open64_DSC_Create_Symbol
                                (const char *name,
                                 Open64_DSC_Handle tensor_type);
extern int Open64_DSC_Attach_Symbol_Metadata
                                (Open64_DSC_Handle symbol,
                                 const Open64_DSC_Attribute *metadata,
                                 unsigned int metadata_count);
extern int Open64_DSC_Attach_Value_Metadata
                                (Open64_DSC_Handle value,
                                 const Open64_DSC_Attribute *metadata,
                                 unsigned int metadata_count);
extern int Open64_DSC_Attach_Value_Lineage
                                (Open64_DSC_Handle value,
                                 const char *lineage);
extern Open64_DSC_Handle Open64_DSC_Get_Value_Type
                                (Open64_DSC_Handle value);
extern Open64_DSC_Handle Open64_DSC_Get_Value_Result_Symbol
                                (Open64_DSC_Handle value);
extern int Open64_DSC_Begin_Program(void);
extern void Open64_DSC_Abort_Program(void);
extern Open64_DSC_Handle Open64_DSC_Create_Minimal_Program_Unit
                                 (const char *name);
extern int Open64_DSC_Select_Program_Unit
                                (Open64_DSC_Handle program_unit);
extern Open64_DSC_Handle Open64_DSC_Declare_PU_Formal
                                (Open64_DSC_Handle program_unit,
                                 const char *name,
                                 unsigned int ordinal,
                                 Open64_DSC_Handle tensor_type,
                                 const Open64_DSC_Source_Position *position);
extern Open64_DSC_Handle Open64_DSC_Declare_PU_Result
                                (Open64_DSC_Handle program_unit,
                                 const char *name,
                                 unsigned int ordinal,
                                 Open64_DSC_Handle tensor_type,
                                 unsigned int role,
                                 const Open64_DSC_Source_Position *position);
extern int Open64_DSC_Return_PU_Values
                                (Open64_DSC_Handle program_unit,
                                 const Open64_DSC_Handle *values,
                                 unsigned int value_count);
extern Open64_DSC_Handle Open64_DSC_Create_PU_Call
                                (Open64_DSC_Handle caller,
                                 Open64_DSC_Handle callee,
                                 const Open64_DSC_Handle *arguments,
                                 unsigned int argument_count,
                                 const char *const *result_names,
                                 unsigned int result_count,
                                 const char *canonical_class_name,
                                 const char *instance_path,
                                 const char *context_identity,
                                 unsigned int call_ordinal,
                                 const Open64_DSC_Source_Position *position);
extern Open64_DSC_Handle Open64_DSC_Get_PU_Call_Result
                                (Open64_DSC_Handle call,
                                 unsigned int ordinal);
extern unsigned int Open64_DSC_Register_Source_File
                                (Open64_DSC_Handle program_unit,
                                 const char *path);
extern int Open64_DSC_Set_Value_Source_Position
                                (Open64_DSC_Handle value,
                                 const Open64_DSC_Source_Position *position);
extern Open64_DSC_Handle Open64_DSC_Create_Region
                                (Open64_DSC_Handle program_unit,
                                 Open64_DSC_Handle parent_region,
                                 const char *contract_name,
                                 unsigned int contract_version);
extern int Open64_DSC_Append_Region_Value
                                (Open64_DSC_Handle region,
                                 Open64_DSC_Handle value);
extern int Open64_DSC_Append_Program_Unit_Region
                                (Open64_DSC_Handle program_unit,
                                 Open64_DSC_Handle region);
extern int Open64_DSC_Append_Child_Region
                                (Open64_DSC_Handle parent_region,
                                 Open64_DSC_Handle child_region);
extern int Open64_DSC_Declare_Region_Value
                                (Open64_DSC_Handle region,
                                 Open64_DSC_Handle value,
                                 unsigned int roles,
                                 unsigned int ordinal,
                                 unsigned int flags);
extern Open64_DSC_Handle Open64_DSC_Declare_State_Object
                                (Open64_DSC_Handle program_unit,
                                 const char *name,
                                 unsigned int kind,
                                 unsigned int flags);
extern int Open64_DSC_Add_State_Effect
                                (Open64_DSC_Handle value,
                                 Open64_DSC_Handle state,
                                 unsigned int effect_kind);
extern int Open64_DSC_Declare_Region_State
                                (Open64_DSC_Handle region,
                                 Open64_DSC_Handle state,
                                 unsigned int effect_kind,
                                 unsigned int ordinal,
                                 unsigned int flags);
extern int Open64_DSC_Set_Region_Source_Position
                                (Open64_DSC_Handle region,
                                 const Open64_DSC_Source_Position *position);
extern int Open64_DSC_Set_Region_Metadata
                                (Open64_DSC_Handle region,
                                 const char *key,
                                 const char *value);
extern int Open64_DSC_Verify_Program(Open64_DSC_Verify_Result *result);
extern int Open64_DSC_Append_Program_Unit_Value
                                (Open64_DSC_Handle program_unit,
                                 Open64_DSC_Handle value);
extern unsigned int Open64_DSC_Count_Program_Unit_Values
                                (Open64_DSC_Handle program_unit);
extern int Open64_DSC_Get_Program_Unit_Value
                                (Open64_DSC_Handle program_unit,
                                 unsigned int index,
                                 Open64_DSC_Value_Info *info);
extern int Open64_DSC_Append_Program_Unit_Marker
                                (Open64_DSC_Handle program_unit,
                                 Open64_DSC_Handle marker);
extern unsigned int Open64_DSC_Count_Program_Unit_Markers
                                (Open64_DSC_Handle program_unit);
extern int Open64_DSC_Get_Program_Unit_Marker
                                (Open64_DSC_Handle program_unit,
                                 unsigned int index,
                                 Open64_DSC_Marker_Info *info);
extern int Open64_DSC_Finalize_Mapped_Image(const char *path);

#ifdef __cplusplus
}
#endif

#endif /* open64_dsc_native_bridge_INCLUDED */
