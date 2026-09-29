/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Common AIO-13 TelemetryProfileIR records and structural services. Runtime
 * instrumentation, profile interpretation, cost refinement, and selection
 * policy belong to the consuming phase. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO13-TELEMETRY-FEEDBACK.md.
 */

#ifndef dsl_telemetry_profile_INCLUDED
#define dsl_telemetry_profile_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_runtime_variant.h"
#include "profile_type.h"
#include "symtab_idx.h"

struct DSL_TELEMETRY_PROFILE_IR;

typedef struct DSL_TELEMETRY_PROFILE_IR DSL_TELEMETRY_PROFILE_IR;
typedef UINT32 DSL_TELEMETRY_RECORD_ID;

#define DSL_TELEMETRY_PROFILE_SCHEMA_VERSION ((UINT32)1)
#define DSL_TELEMETRY_OCCUPANCY_SCALE ((UINT32)1000000)

typedef enum {
    DSL_TELEMETRY_PRODUCER_UNKNOWN = 0,
    DSL_TELEMETRY_PRODUCER_OPEN64_FEEDBACK = 1,
    DSL_TELEMETRY_PRODUCER_RUNTIME = 2,
    DSL_TELEMETRY_PRODUCER_BENCHMARK = 3
} DSL_TELEMETRY_PRODUCER_KIND;

enum {
    DSL_TELEMETRY_RECORD_FLAG_NONE = 0,
    DSL_TELEMETRY_RECORD_FLAG_COMPLETE = 0x00000001
};

typedef struct {
    UINT32 schema_version;
    UINT32 producer_kind;
    UINT32 profile_generation;
    UINT32 target_profile_id;
    ST_IDX owner_pu_st;
    UINT32 instrumentation_phase;
    UINT32 record_count;
    UINT32 reserved;
} DSL_TELEMETRY_PROFILE_HEADER;

typedef struct {
    DSL_TELEMETRY_RECORD_ID id;
    DSL_RUNTIME_VARIANT_SITE_ID site_id;
    DSL_RUNTIME_VARIANT_ID variant_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    UINT64 variant_identity;
    UINT64 sample_count;
    UINT64 selected_count;
    UINT64 guard_evaluation_count;
    UINT64 guard_pass_count;
    UINT64 latency_ns_total;
    UINT64 latency_ns_min;
    UINT64 latency_ns_max;
    UINT32 achieved_occupancy_ppm;
    UINT32 reserved0;
    UINT64 memory_read_bytes;
    UINT64 memory_write_bytes;
    UINT64 communication_ns_total;
    UINT64 overlap_ns_total;
    UINT64 cache_hit_count;
    UINT64 cache_miss_count;
    UINT64 launch_ns_total;
    UINT32 flags;
    UINT32 reserved1;
} DSL_TELEMETRY_RECORD;

typedef struct {
    DSL_TELEMETRY_PROFILE_HEADER header;
    const DSL_TELEMETRY_RECORD *records;
    UINT32 record_count;
} DSL_TELEMETRY_PROFILE_CREATE_INFO;

extern DSL_TELEMETRY_PROFILE_IR *DSL_telemetry_profile_create
                                (const DSL_TELEMETRY_PROFILE_CREATE_INFO *info,
                                 FILE *diagnostic);
extern void DSL_telemetry_profile_destroy
                                (DSL_TELEMETRY_PROFILE_IR *profile);
extern BOOL DSL_telemetry_profile_verify
                                (const DSL_TELEMETRY_PROFILE_IR *profile,
                                 FILE *diagnostic);
extern void DSL_telemetry_profile_print
                                (FILE *file,
                                 const DSL_TELEMETRY_PROFILE_IR *profile);
extern BOOL DSL_telemetry_profile_get_header
                                (const DSL_TELEMETRY_PROFILE_IR *profile,
                                 DSL_TELEMETRY_PROFILE_HEADER *header);
extern UINT32 DSL_telemetry_profile_record_count
                                (const DSL_TELEMETRY_PROFILE_IR *profile);
extern BOOL DSL_telemetry_profile_get_record
                                (const DSL_TELEMETRY_PROFILE_IR *profile,
                                 DSL_TELEMETRY_RECORD_ID id,
                                 DSL_TELEMETRY_RECORD *record);
extern const char *DSL_telemetry_producer_name (UINT32 producer_kind);

#endif /* dsl_telemetry_profile_INCLUDED */
