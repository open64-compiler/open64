/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * TelemetryProfileIR storage, structural verification, access, and generic
 * printing. VHO owns freshness checks and optimization policy.
 */

#include <string.h>
#include <vector>

#include "dsl_telemetry_profile.h"

struct DSL_TELEMETRY_PROFILE_IR {
    DSL_TELEMETRY_PROFILE_HEADER header;
    std::vector<DSL_TELEMETRY_RECORD> records;
};

static const char *DSL_telemetry_producer_name_table[] = {
    "unknown", "open64_feedback", "runtime", "benchmark"
};

static BOOL
DSL_Telemetry_Profile_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "TelemetryProfileIR error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Telemetry_Multiply
        (UINT64 left, UINT64 right, UINT64 *result)
{
    if (result == NULL || (left != 0 && right > ~(UINT64)0 / left))
        return FALSE;
    *result = left * right;
    return TRUE;
}

const char *
DSL_telemetry_producer_name (UINT32 producer_kind)
{
    return producer_kind < sizeof(DSL_telemetry_producer_name_table) /
                               sizeof(DSL_telemetry_producer_name_table[0]) ?
           DSL_telemetry_producer_name_table[producer_kind] : "unknown";
}

BOOL
DSL_telemetry_profile_verify
        (const DSL_TELEMETRY_PROFILE_IR *profile, FILE *diagnostic)
{
    if (profile == NULL ||
        profile->header.schema_version !=
            DSL_TELEMETRY_PROFILE_SCHEMA_VERSION ||
        profile->header.producer_kind <= DSL_TELEMETRY_PRODUCER_UNKNOWN ||
        profile->header.producer_kind > DSL_TELEMETRY_PRODUCER_BENCHMARK ||
        profile->header.profile_generation == 0 ||
        profile->header.target_profile_id == 0 ||
        profile->header.owner_pu_st == ST_IDX_ZERO ||
        profile->header.instrumentation_phase != PROFILE_PHASE_BEFORE_VHO ||
        profile->header.record_count != profile->records.size() ||
        profile->header.reserved != 0)
        return DSL_Telemetry_Profile_Report
                   (diagnostic, "invalid profile header", 0);

    for (UINT32 i = 0; i < profile->records.size(); ++i) {
        const DSL_TELEMETRY_RECORD &record = profile->records[i];
        UINT64 minimum_total;
        UINT64 maximum_total;
        if (!DSL_Telemetry_Multiply
                 (record.latency_ns_min, record.sample_count,
                  &minimum_total) ||
            !DSL_Telemetry_Multiply
                 (record.latency_ns_max, record.sample_count,
                  &maximum_total) ||
            record.id != i + 1 || record.site_id == 0 ||
            record.variant_id == 0 || record.optimization_plan_id == 0 ||
            record.variant_identity == 0 || record.sample_count == 0 ||
            record.selected_count > record.sample_count ||
            record.guard_pass_count > record.guard_evaluation_count ||
            record.guard_evaluation_count >
                ~(UINT64)0 / DSL_TELEMETRY_OCCUPANCY_SCALE ||
            record.latency_ns_min == 0 ||
            record.latency_ns_max < record.latency_ns_min ||
            record.latency_ns_total < minimum_total ||
            record.latency_ns_total > maximum_total ||
            record.achieved_occupancy_ppm >
                DSL_TELEMETRY_OCCUPANCY_SCALE ||
            record.overlap_ns_total > record.communication_ns_total ||
            record.launch_ns_total > record.latency_ns_total ||
            record.flags != DSL_TELEMETRY_RECORD_FLAG_COMPLETE ||
            record.reserved0 != 0 || record.reserved1 != 0)
            return DSL_Telemetry_Profile_Report
                       (diagnostic, "invalid telemetry record", record.id);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_TELEMETRY_RECORD &prior = profile->records[j];
            if (prior.site_id == record.site_id &&
                prior.variant_id == record.variant_id)
                return DSL_Telemetry_Profile_Report
                           (diagnostic, "duplicate variant record", record.id);
        }
    }
    return TRUE;
}

DSL_TELEMETRY_PROFILE_IR *
DSL_telemetry_profile_create
        (const DSL_TELEMETRY_PROFILE_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->header.record_count != info->record_count ||
        (info->record_count != 0 && info->records == NULL)) {
        DSL_Telemetry_Profile_Report
            (diagnostic, "invalid create request", 0);
        return NULL;
    }
    DSL_TELEMETRY_PROFILE_IR *profile = new DSL_TELEMETRY_PROFILE_IR;
    profile->header = info->header;
    if (info->record_count != 0)
        profile->records.assign(info->records,
                                info->records + info->record_count);
    if (!DSL_telemetry_profile_verify(profile, diagnostic)) {
        delete profile;
        return NULL;
    }
    return profile;
}

void
DSL_telemetry_profile_destroy (DSL_TELEMETRY_PROFILE_IR *profile)
{
    delete profile;
}

void
DSL_telemetry_profile_print
        (FILE *file, const DSL_TELEMETRY_PROFILE_IR *profile)
{
    if (file == NULL || profile == NULL)
        return;
    fprintf(file,
            "TelemetryProfileIR: schema=%u producer=%s generation=%u "
            "target=%u owner=0x%x phase=%u records=%u\n",
            profile->header.schema_version,
            DSL_telemetry_producer_name(profile->header.producer_kind),
            profile->header.profile_generation,
            profile->header.target_profile_id,
            profile->header.owner_pu_st,
            profile->header.instrumentation_phase,
            profile->header.record_count);
    for (UINT32 i = 0; i < profile->records.size(); ++i) {
        const DSL_TELEMETRY_RECORD &record = profile->records[i];
        fprintf(file,
                "  telemetry id=%u site=%u variant=%u plan=%u "
                "identity=0x%llx samples=%llu selected=%llu "
                "guard=%llu/%llu latency_ns=%llu min=%llu max=%llu "
                "occupancy_ppm=%u memory_read=%llu memory_write=%llu "
                "communication_ns=%llu overlap_ns=%llu cache=%llu/%llu "
                "launch_ns=%llu\n",
                record.id, record.site_id, record.variant_id,
                record.optimization_plan_id,
                (unsigned long long)record.variant_identity,
                (unsigned long long)record.sample_count,
                (unsigned long long)record.selected_count,
                (unsigned long long)record.guard_pass_count,
                (unsigned long long)record.guard_evaluation_count,
                (unsigned long long)record.latency_ns_total,
                (unsigned long long)record.latency_ns_min,
                (unsigned long long)record.latency_ns_max,
                record.achieved_occupancy_ppm,
                (unsigned long long)record.memory_read_bytes,
                (unsigned long long)record.memory_write_bytes,
                (unsigned long long)record.communication_ns_total,
                (unsigned long long)record.overlap_ns_total,
                (unsigned long long)record.cache_hit_count,
                (unsigned long long)record.cache_miss_count,
                (unsigned long long)record.launch_ns_total);
    }
}

BOOL
DSL_telemetry_profile_get_header
        (const DSL_TELEMETRY_PROFILE_IR *profile,
         DSL_TELEMETRY_PROFILE_HEADER *header)
{
    if (profile == NULL || header == NULL)
        return FALSE;
    *header = profile->header;
    return TRUE;
}

UINT32
DSL_telemetry_profile_record_count
        (const DSL_TELEMETRY_PROFILE_IR *profile)
{
    return profile == NULL ? 0 : profile->records.size();
}

BOOL
DSL_telemetry_profile_get_record
        (const DSL_TELEMETRY_PROFILE_IR *profile,
         DSL_TELEMETRY_RECORD_ID id, DSL_TELEMETRY_RECORD *record)
{
    if (profile == NULL || record == NULL || id == 0 ||
        id > profile->records.size())
        return FALSE;
    *record = profile->records[id - 1];
    return TRUE;
}
