/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_ir_image_INCLUDED
#define dsl_ir_image_INCLUDED

#include "defs.h"

/*
 * Source-level DSL binary image probe.
 *
 * These constants describe the future fixed-row records that may be carried in
 * the reserved WT_DSL_TENSOR_DESCRIPTOR section.  They are intentionally not
 * connected to ir_bread.cxx or ir_bwrite.cxx yet, so the current compiler does
 * not emit, read, or require a new binary WHIRL section.
 */

#define DSL_IR_IMAGE_VERSION 1

typedef enum {
    DSL_IR_IMAGE_RECORD_UNKNOWN = 0,
    DSL_IR_IMAGE_RECORD_TENSOR_DESCRIPTOR = 1,
    DSL_IR_IMAGE_RECORD_TENSOR_TYPE_CORE = 2,
    DSL_IR_IMAGE_RECORD_TENSOR_TRAIT_SET = 3,
    DSL_IR_IMAGE_RECORD_TENSOR_REPRESENTATION = 4,
    DSL_IR_IMAGE_RECORD_TENSOR_LINEAGE = 5
} DSL_IR_IMAGE_RECORD_KIND;

typedef struct {
    UINT32 version;
    UINT32 record_kind_count;
    UINT32 flags;
} DSL_IR_IMAGE_HEADER;

#endif /* dsl_ir_image_INCLUDED */
