/*
 * Copyright (C) 2026 Open64 Project
 *
 * Typed source-event to executable CKKS-step relation. See
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef dsl_ckks_event_INCLUDED
#define dsl_ckks_event_INCLUDED

#include <stdio.h>

#include "dsl_ir_image.h"

#define DSL_CKKS_EVENT_IMAGE_MAGIC       0x44434b45
#define DSL_CKKS_EVENT_IMAGE_VERSION     1
#define DSL_CKKS_EVENT_IMAGE_HEADER_SIZE 32
#define DSL_CKKS_EVENT_RECORD_SIZE       64
#define DSL_CKKS_EVENT_INVALID_ID        0
#define DSL_CKKS_EVENT_FINAL_RESULT      0x00000001

typedef UINT32 DSL_CKKS_EVENT_ID;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_size;
    UINT32 record_count;
    UINT32 capabilities;
    UINT32 flags;
    UINT32 reserved;
} DSL_CKKS_EVENT_IMAGE_HEADER;

typedef struct {
    DSL_CKKS_EVENT_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    DSL_IR_NODE_ID source_node_id;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    UINT32 source_static_ordinal;
    UINT32 step_ordinal;
    DSL_IR_VALUE_ID result_value_id;
    DSL_IR_NODE_ID result_node_id;
    ST_IDX origin_owner_pu_st;
    DSL_IR_VALUE_ID origin_source_value_id;
    UINT32 origin_static_ordinal;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_CKKS_EVENT_RECORD;

extern void DSL_CKKS_Event_Image_Reset (void);
extern BOOL DSL_CKKS_Event_Image_Has_Records (void);
extern UINT32 DSL_CKKS_Event_Image_Count (void);
extern void DSL_CKKS_Event_Image_Get_Header
                                (DSL_CKKS_EVENT_IMAGE_HEADER *header);
extern BOOL DSL_CKKS_Event_Image_Get
                                (DSL_CKKS_EVENT_ID id,
                                 DSL_CKKS_EVENT_RECORD *record);
extern BOOL DSL_CKKS_Event_Image_Has_Source
                                (DSL_IR_VALUE_ID source_value_id);
extern BOOL DSL_CKKS_Event_Image_Validate (FILE *diagnostic);
extern BOOL DSL_CKKS_Event_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
extern void DSL_CKKS_Event_Image_Print (FILE *file);

#endif /* dsl_ckks_event_INCLUDED */
