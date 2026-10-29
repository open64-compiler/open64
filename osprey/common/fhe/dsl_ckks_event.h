/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/fhe exposes the typed source-event to executable CKKS-step image
 * relation for producers, mapped readers/writers and inspection. The existing
 * v1 row order, sizes and .B section contract are preserved.
 * Design: doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
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

/* Discard all managed event rows for the image lifecycle. Existing IDs lose
 * meaning; no source/result WHIRL rows or caller buffers are modified. */
extern void DSL_CKKS_Event_Image_Reset (void);
/* Read whether the managed event table is nonempty without mutation. */
extern BOOL DSL_CKKS_Event_Image_Has_Records (void);
/* Read the managed event row count without mutation. */
extern UINT32 DSL_CKKS_Event_Image_Count (void);
/* Fill a caller-owned v1 header with current count, fixed sizes and capability
 * bit, clearing flags/reserves; NULL is a no-op. No table state is changed. */
extern void DSL_CKKS_Event_Image_Get_Header
                                (DSL_CKKS_EVENT_IMAGE_HEADER *header);
/* Copy a one-based managed row into caller storage. Invalid ID or NULL output
 * returns FALSE without writes or mutation. */
extern BOOL DSL_CKKS_Event_Image_Get
                                (DSL_CKKS_EVENT_ID id,
                                 DSL_CKKS_EVENT_RECORD *record);
/* Test exact source-value membership by scanning managed rows; no ownership,
 * context or semantic validation is implied and no state is changed. */
extern BOOL DSL_CKKS_Event_Image_Has_Source
                                (DSL_IR_VALUE_ID source_value_id);
/* Validate current event rows against managed DSL values/nodes and call
 * contexts without mutation. Return FALSE with optional diagnostics for an
 * invalid relation; FHE gatekeeping separately proves complete source coverage. */
extern BOOL DSL_CKKS_Event_Image_Validate (FILE *diagnostic);
/* Borrow an aligned section view for this call and validate its exact size,
 * v1 header and relations against already loaded DSL/call tables before copying
 * rows. Rejection preserves the old table; (NULL, 0) clears it successfully.
 * Allocation failures are not caught or rolled back after validation. */
extern BOOL DSL_CKKS_Event_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
/* Print current managed rows in ID order to the caller-owned stream without
 * mutation. NULL or an empty table is a no-op; stream errors are not reported. */
extern void DSL_CKKS_Event_Image_Print (FILE *file);

#endif /* dsl_ckks_event_INCLUDED */
