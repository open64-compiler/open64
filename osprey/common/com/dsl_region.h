/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_region_INCLUDED
#define dsl_region_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "pu_info.h"
#include "symtab_idx.h"
#include "wn.h"
#include "wn_map.h"

#define DSL_REGION_IMAGE_MAGIC   0x44534c52
#define DSL_REGION_IMAGE_VERSION 1

typedef struct dsl_region_runtime *DSL_REGION;

typedef enum {
    DSL_REGION_VALUE_INPUT = 0x00000001,
    DSL_REGION_VALUE_OUTPUT = 0x00000002,
    DSL_REGION_VALUE_INOUT = 0x00000004,
    DSL_REGION_VALUE_RESULT = 0x00000008
} DSL_REGION_VALUE_ROLE;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 region_count;
    UINT32 interface_count;
    UINT32 flags;
    UINT32 reserved;
} DSL_REGION_IMAGE_HEADER;

typedef struct {
    UINT32 region_id;
    UINT32 parent_region_id;
    UINT32 kind;
    UINT32 depth;
    UINT32 wn_offset;
    UINT32 contract_version;
    STR_IDX contract_name;
    UINT32 flags;
} DSL_REGION_IMAGE_RECORD;

typedef struct {
    UINT32 region_id;
    UINT32 ordinal;
    ST_IDX st;
    UINT32 roles;
    UINT32 flags;
    UINT32 reserved;
} DSL_REGION_INTERFACE_RECORD;

extern void DSL_Region_Reset (void);
extern DSL_REGION DSL_Region_Create (PU_Info *pu, DSL_REGION parent,
                                     const char *contract_name,
                                     UINT32 contract_version);
extern BOOL DSL_Region_Append_Statement (DSL_REGION region, WN *statement);
extern BOOL DSL_Region_Append_Child (DSL_REGION parent, DSL_REGION child);
extern BOOL DSL_Region_Append_To_PU (DSL_REGION region);
extern BOOL DSL_Region_Set_Metadata (DSL_REGION region, const char *key,
                                     const char *value);
extern BOOL DSL_Region_Declare_Symbol (DSL_REGION region, ST_IDX st,
                                      UINT32 roles, UINT32 ordinal,
                                      UINT32 flags);
extern BOOL DSL_Region_Set_Source_Position (DSL_REGION region, SRCPOS spos);
extern WN *DSL_Region_WN (DSL_REGION region);
extern BOOL DSL_Region_Is_Managed_WN (PU_Info *pu, const WN *wn);
extern BOOL DSL_Region_Consume_WN (PU_Info *pu, const WN *wn);
extern BOOL DSL_Region_Verify_PU (PU_Info *pu, FILE *diagnostic);
extern void DSL_Region_Print_PU (FILE *file, PU_Info *pu);

struct output_file;
extern BOOL DSL_Region_Write_PU (PU_Info *pu, WN_MAP off_map,
                                 struct output_file *output);
extern INT DSL_Region_Load_Mapped_PU (PU_Info *pu, const void *tree_base,
                                      UINT64 tree_size, const void *section_base,
                                      UINT64 section_size);

#endif /* dsl_region_INCLUDED */
