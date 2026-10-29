/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Persisted WT_REGIONS schema owned by common/com: pointer-free image rows,
 * roles, flags and encoding constants only; no live WN/PU/store ownership.
 * Field order, widths, IDs and version remain the existing .B ABI; actual
 * 24/40/24 size checks belong to common/ir_image/dsl_region_image.cxx.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#ifndef dsl_region_INCLUDED
#define dsl_region_INCLUDED

#include "defs.h"
#include "symtab_idx.h"

#define DSL_REGION_IMAGE_MAGIC   0x44534c52
#define DSL_REGION_IMAGE_VERSION 1

typedef enum {
    DSL_REGION_VALUE_INPUT = 0x00000001,
    DSL_REGION_VALUE_OUTPUT = 0x00000002,
    DSL_REGION_VALUE_INOUT = 0x00000004,
    DSL_REGION_VALUE_RESULT = 0x00000008
} DSL_REGION_VALUE_ROLE;

typedef enum {
    DSL_REGION_INTERFACE_FLAG_NONE = 0,
    DSL_REGION_INTERFACE_ABSTRACT_STATE = 0x00000001,
    DSL_REGION_INTERFACE_UNIQUE_OWNERSHIP = 0x00000002,
    DSL_REGION_INTERFACE_STATE_READ = 0x00000004,
    DSL_REGION_INTERFACE_STATE_MODIFY = 0x00000008,
    DSL_REGION_INTERFACE_LAYER_OWNED = 0x00000010
} DSL_REGION_INTERFACE_FLAG;

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

#endif /* dsl_region_INCLUDED */
