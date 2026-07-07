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

extern Open64_DSC_Handle Open64_DSC_Create_Tensor_Type
                                (const char *name,
                                 const char *dtype,
                                 int rank,
                                 const char *logical_shape);
extern Open64_DSC_Handle Open64_DSC_Create_Tensor_Constant
                                (const char *name,
                                 const char *dtype,
                                 unsigned int rank,
                                 const char *logical_shape,
                                 const char *value_kind,
                                 const char *value);
extern Open64_DSC_Handle Open64_DSC_Create_Operator
                                (const char *opcode_name,
                                 unsigned int version,
                                 const Open64_DSC_Handle *kids,
                                 unsigned int kid_count,
                                 const Open64_DSC_Attribute *attrs,
                                 unsigned int attr_count);
extern int Open64_DSC_Finalize_Mapped_Image(const char *path);

#ifdef __cplusplus
}
#endif

#endif /* open64_dsc_native_bridge_INCLUDED */
