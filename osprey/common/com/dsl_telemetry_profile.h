/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Common AIO-13 TelemetryProfileIR records and structural services. Runtime
 * instrumentation, profile interpretation, cost refinement, and selection
 * policy belong to the consuming phase. This general service is intended for
 * cross-session data, but currently owns only in-memory copies and text dumps;
 * it adds no .B rows, feedback-file format or storage/transport implementation.
 * No active PU context is read or changed; IDs are labels, not store handles.
 * See doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO13-TELEMETRY-FEEDBACK.md.
 */

#ifndef dsl_telemetry_profile_INCLUDED
#define dsl_telemetry_profile_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_plan_identity.h"
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

/* Version 1 requires a known nonzero producer, generation, target and owner,
 * PROFILE_PHASE_BEFORE_VHO, exact record count and zero reserved field. Owner
 * is only a nonzero label here; semantic ownership/freshness is phase-owned. */
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

/* Immutable copied measurement row: IDs are contiguous 1-based, site/variant
 * pairs unique, all identity labels nonzero, sample count positive. Units are
 * integer ns, bytes and occupancy ppm (0..1000000). COMPLETE is the only valid
 * flags value; reserved fields are zero. Verification checks bounded counters,
 * latency products/ranges and overlap/launch containment, not measured truth,
 * coverage of any plan store, target support or profitability. */
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

/* Borrow records only for create(); both counts must agree. Creation deep
 * copies the rows, so callers retain ownership and may release input afterward. */
typedef struct {
    DSL_TELEMETRY_PROFILE_HEADER header;
    const DSL_TELEMETRY_RECORD *records;
    UINT32 record_count;
} DSL_TELEMETRY_PROFILE_CREATE_INFO;

/* Allocate an owned immutable profile by copying info and its rows, then verify.
 * Bad input returns NULL, optionally diagnoses, and deletes a rejected copy.
 * Allocation exceptions are not caught/rolled back here. Caller destroys a
 * successful result; no caller data, IR, active context or plan store changes. */
extern DSL_TELEMETRY_PROFILE_IR *DSL_telemetry_profile_create
                                (const DSL_TELEMETRY_PROFILE_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release a create() result and its owned rows; NULL is harmless. All borrowed
 * profile references become invalid; no compiler context or external IDs change. */
extern void DSL_telemetry_profile_destroy
                                (DSL_TELEMETRY_PROFILE_IR *profile);
/* Read-only schema/range/uniqueness verification, with optional first-error
 * diagnostic. NULL fails. Does not join current PU/plan/target state, validate
 * freshness, infer missing measurements, or select a future implementation. */
extern BOOL DSL_telemetry_profile_verify
                                (const DSL_TELEMETRY_PROFILE_IR *profile,
                                 FILE *diagnostic);
/* Borrow profile and stream to emit generic diagnostic text without validation
 * or ownership transfer. NULL stream/profile is a no-op; I/O errors are not
 * reported and output can be partial. This is not a persistent transport. */
extern void DSL_telemetry_profile_print
                                (FILE *file,
                                 const DSL_TELEMETRY_PROFILE_IR *profile);
/* Copy the immutable header to caller storage; NULL arguments return FALSE
 * without writing output. Does not reverify the profile or current context. */
extern BOOL DSL_telemetry_profile_get_header
                                (const DSL_TELEMETRY_PROFILE_IR *profile,
                                 DSL_TELEMETRY_PROFILE_HEADER *header);
/* Read the owned row count, or zero for NULL, without validation or mutation. */
extern UINT32 DSL_telemetry_profile_record_count
                                (const DSL_TELEMETRY_PROFILE_IR *profile);
/* Copy the row at 1-based id to caller storage without joining external stores.
 * NULL arguments or out-of-range id return FALSE and preserve output. */
extern BOOL DSL_telemetry_profile_get_record
                                (const DSL_TELEMETRY_PROFILE_IR *profile,
                                 DSL_TELEMETRY_RECORD_ID id,
                                 DSL_TELEMETRY_RECORD *record);
/* Borrow a static producer spelling for process lifetime; unknown or out-of-
 * range values yield "unknown" and do not validate a profile producer. */
extern const char *DSL_telemetry_producer_name (UINT32 producer_kind);

#endif /* dsl_telemetry_profile_INCLUDED */
