/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO-13 profile freshness checks, telemetry
 * interpretation, measured-cost construction, and future-plan recommendation.
 * Common/com owns only TelemetryProfileIR records and structural services.
 */

#ifndef dsl_telemetry_feedback_opt_INCLUDED
#define dsl_telemetry_feedback_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_runtime_variant_opt.h"
#include "dsl_telemetry_profile.h"

struct pu_info;
struct DSL_TELEMETRY_FEEDBACK_ANALYSIS;

typedef struct DSL_TELEMETRY_FEEDBACK_ANALYSIS
    DSL_TELEMETRY_FEEDBACK_ANALYSIS;

typedef enum {
    VHO_DSL_TELEMETRY_STATUS_UNKNOWN = 0,
    VHO_DSL_TELEMETRY_STATUS_NO_PROFILE = 1,
    VHO_DSL_TELEMETRY_STATUS_APPLIED = 2
} VHO_DSL_TELEMETRY_STATUS;

typedef struct {
    UINT32 consume_profile;
    UINT32 target_profile_id;
    UINT32 expected_profile_generation;
    UINT32 require_complete_profile;
    UINT32 max_records;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} VHO_DSL_TELEMETRY_FEEDBACK_CONTROL;

typedef struct {
    DSL_RUNTIME_VARIANT_SITE_ID site_id;
    DSL_RUNTIME_VARIANT_ID variant_id;
    DSL_OPT_PLAN_ID source_plan_id;
    DSL_OPT_PLAN_ID feedback_plan_id;
    UINT64 average_latency_ns;
    UINT32 guard_hit_rate_ppm;
    UINT32 originally_selected;
    UINT32 recommended;
    UINT32 reserved;
} VHO_DSL_TELEMETRY_VARIANT_RESULT;

typedef struct {
    DSL_RUNTIME_VARIANT_SITE_ID site_id;
    DSL_RUNTIME_VARIANT_ID original_variant_id;
    DSL_RUNTIME_VARIANT_ID recommended_variant_id;
    DSL_OPT_PLAN_ID recommended_plan_id;
    UINT32 profitability_changed;
    UINT32 legality_changed;
} VHO_DSL_TELEMETRY_SITE_RESULT;

extern void VHO_DSL_Telemetry_Feedback_Control_Init
                                (VHO_DSL_TELEMETRY_FEEDBACK_CONTROL *control);
extern DSL_TELEMETRY_FEEDBACK_ANALYSIS *VHO_DSL_Telemetry_Feedback_Create
                                (struct pu_info *pu,
                                 const DSL_RUNTIME_VARIANT_ANALYSIS *runtime,
                                 const DSL_TELEMETRY_PROFILE_IR *profile,
                                 const VHO_DSL_TELEMETRY_FEEDBACK_CONTROL
                                     *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Telemetry_Feedback_Destroy
                                (DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis);
extern BOOL VHO_DSL_Telemetry_Feedback_Build
                                (DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Telemetry_Feedback_Verify
                                (const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Telemetry_Feedback_Print
                                (FILE *file,
                                 const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis);
extern UINT32 VHO_DSL_Telemetry_Feedback_Status
                                (const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis);
extern UINT32 VHO_DSL_Telemetry_Feedback_Variant_Count
                                (const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis);
extern UINT32 VHO_DSL_Telemetry_Feedback_Site_Count
                                (const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis);
extern BOOL VHO_DSL_Telemetry_Feedback_Get_Variant
                                (const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis,
                                 UINT32 ordinal,
                                 VHO_DSL_TELEMETRY_VARIANT_RESULT *result);
extern BOOL VHO_DSL_Telemetry_Feedback_Get_Site
                                (const DSL_TELEMETRY_FEEDBACK_ANALYSIS
                                     *analysis,
                                 DSL_RUNTIME_VARIANT_SITE_ID site_id,
                                 VHO_DSL_TELEMETRY_SITE_RESULT *result);

#endif /* dsl_telemetry_feedback_opt_INCLUDED */
