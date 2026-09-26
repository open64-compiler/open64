/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Policy-free CommonPhysicalPlanIR storage, structural verification, provider
 * capability records, and inspection. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md.
 */

#include <string.h>
#include <vector>

#include "dsl_physical_plan.h"
#include "dsl_opcode.h"
#include "mtypes.h"

struct DSL_PHYSICAL_PLAN_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_PHYSICAL_SITE_RECORD> sites;
    std::vector<DSL_PHYSICAL_IMPLEMENTATION_RECORD> implementations;
};

static const char *DSL_physical_provider_name_table[] = {
    "unknown", "open64_direct", "open64_generated", "nvidia_cublaslt",
    "nvidia_cudnn", "triton", "existing_ptx"
};

static const char *DSL_physical_implementation_name_table[] = {
    "unknown", "baseline_direct", "generated_kernel", "library",
    "existing_kernel"
};

static const char *DSL_physical_schedule_name_table[] = {
    "unknown", "direct", "tiled_pipeline", "provider_owned"
};

static const DSL_PROVIDER_CAPABILITY_RECORD DSL_provider_capability_table[] = {
    { 1, DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
      DSL_PHYSICAL_IMPLEMENTATION_BASELINE_DIRECT,
      DSL_TARGET_PROFILE_UNKNOWN, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_DIRECT,
      DSL_PROVIDER_CAPABILITY_FLAG_BUILTIN |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 2, DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED,
      DSL_PHYSICAL_IMPLEMENTATION_GENERATED_KERNEL,
      DSL_TARGET_PROFILE_NVIDIA_HOPPER, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_TILED_PIPELINE,
      DSL_PROVIDER_CAPABILITY_FLAG_BUILTIN |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 3, DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED,
      DSL_PHYSICAL_IMPLEMENTATION_GENERATED_KERNEL,
      DSL_TARGET_PROFILE_NVIDIA_BLACKWELL, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_TILED_PIPELINE,
      DSL_PROVIDER_CAPABILITY_FLAG_BUILTIN |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 4, DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT,
      DSL_PHYSICAL_IMPLEMENTATION_LIBRARY,
      DSL_TARGET_PROFILE_NVIDIA_HOPPER, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 5, DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT,
      DSL_PHYSICAL_IMPLEMENTATION_LIBRARY,
      DSL_TARGET_PROFILE_NVIDIA_BLACKWELL, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 6, DSL_PHYSICAL_PROVIDER_NVIDIA_CUDNN,
      DSL_PHYSICAL_IMPLEMENTATION_LIBRARY,
      DSL_TARGET_PROFILE_NVIDIA_HOPPER, OPR_DSLCONV2D, 2, MTYPE_F4, 4,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 7, DSL_PHYSICAL_PROVIDER_NVIDIA_CUDNN,
      DSL_PHYSICAL_IMPLEMENTATION_LIBRARY,
      DSL_TARGET_PROFILE_NVIDIA_BLACKWELL, OPR_DSLCONV2D, 2, MTYPE_F4, 4,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 8, DSL_PHYSICAL_PROVIDER_TRITON,
      DSL_PHYSICAL_IMPLEMENTATION_EXISTING_KERNEL,
      DSL_TARGET_PROFILE_NVIDIA_HOPPER, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 9, DSL_PHYSICAL_PROVIDER_TRITON,
      DSL_PHYSICAL_IMPLEMENTATION_EXISTING_KERNEL,
      DSL_TARGET_PROFILE_NVIDIA_BLACKWELL, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 10, DSL_PHYSICAL_PROVIDER_EXISTING_PTX,
      DSL_PHYSICAL_IMPLEMENTATION_EXISTING_KERNEL,
      DSL_TARGET_PROFILE_NVIDIA_HOPPER, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 },
    { 11, DSL_PHYSICAL_PROVIDER_EXISTING_PTX,
      DSL_PHYSICAL_IMPLEMENTATION_EXISTING_KERNEL,
      DSL_TARGET_PROFILE_NVIDIA_BLACKWELL, OPR_DSLMATMUL, 1, MTYPE_F4, 2,
      DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED,
      DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
      DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED, 0, 0 }
};

static BOOL
DSL_Physical_IR_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CommonPhysicalPlanIR error: %s id=%u\n",
                message, id);
    return FALSE;
}

const char *
DSL_physical_provider_name (UINT32 provider)
{
    return provider < sizeof(DSL_physical_provider_name_table) /
                          sizeof(DSL_physical_provider_name_table[0]) ?
           DSL_physical_provider_name_table[provider] : "unknown";
}

const char *
DSL_physical_implementation_name (UINT32 implementation)
{
    return implementation <
               sizeof(DSL_physical_implementation_name_table) /
               sizeof(DSL_physical_implementation_name_table[0]) ?
           DSL_physical_implementation_name_table[implementation] :
           "unknown";
}

const char *
DSL_physical_schedule_name (UINT32 schedule)
{
    return schedule < sizeof(DSL_physical_schedule_name_table) /
                          sizeof(DSL_physical_schedule_name_table[0]) ?
           DSL_physical_schedule_name_table[schedule] : "unknown";
}

UINT32
DSL_provider_capability_count (void)
{
    return sizeof(DSL_provider_capability_table) /
           sizeof(DSL_provider_capability_table[0]);
}

BOOL
DSL_provider_capability_get
        (DSL_PROVIDER_CAPABILITY_ID id,
         DSL_PROVIDER_CAPABILITY_RECORD *record)
{
    if (record == NULL || id == 0 || id > DSL_provider_capability_count())
        return FALSE;
    *record = DSL_provider_capability_table[id - 1];
    return TRUE;
}

DSL_PHYSICAL_PLAN_IR *
DSL_physical_plan_ir_create
        (const DSL_PHYSICAL_PLAN_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->implementation_count != 0 &&
         info->implementations == NULL)) {
        DSL_Physical_IR_Report(diagnostic, "invalid create request", 0);
        return NULL;
    }
    DSL_PHYSICAL_PLAN_IR *ir = new DSL_PHYSICAL_PLAN_IR;
    ir->owner_pu_st = info->owner_pu_st;
    if (info->site_count != 0)
        ir->sites.assign(info->sites, info->sites + info->site_count);
    if (info->implementation_count != 0)
        ir->implementations.assign
            (info->implementations,
             info->implementations + info->implementation_count);
    if (!DSL_physical_plan_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void
DSL_physical_plan_ir_destroy (DSL_PHYSICAL_PLAN_IR *ir)
{
    delete ir;
}

BOOL
DSL_physical_plan_ir_verify
        (const DSL_PHYSICAL_PLAN_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Physical_IR_Report(diagnostic, "invalid IR", 0);
    UINT32 expected_implementation = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_PHYSICAL_SITE_RECORD &site = ir->sites[i];
        UINT32 selected_count = 0;
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_node_id == DSL_IR_NODE_INVALID_ID ||
            site.semantic_value_id == DSL_IR_VALUE_INVALID_ID ||
            site.first_implementation_id != expected_implementation ||
            site.implementation_count == 0 ||
            site.first_implementation_id > ir->implementations.size() ||
            site.implementation_count > ir->implementations.size() -
                site.first_implementation_id + 1 ||
            site.baseline_implementation_id != expected_implementation ||
            (site.selected_implementation_id != 0 &&
             (site.selected_implementation_id < expected_implementation ||
              site.selected_implementation_id >=
                  expected_implementation + site.implementation_count)) ||
            ((site.selected_implementation_id == 0) !=
             (site.selected_plan_id == DSL_OPT_PLAN_INVALID_ID)) ||
            site.reserved != 0)
            return DSL_Physical_IR_Report
                       (diagnostic, "invalid site", site.id);
        for (UINT32 j = 0; j < site.implementation_count; ++j) {
            const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation =
                ir->implementations[expected_implementation - 1];
            DSL_PROVIDER_CAPABILITY_RECORD capability;
            BOOL selected = implementation.id ==
                            site.selected_implementation_id;
            BOOL baseline = j == 0;
            UINT64 subtotal = implementation.compute_cost +
                implementation.memory_cost;
            if (subtotal < implementation.compute_cost)
                return DSL_Physical_IR_Report
                           (diagnostic, "cost overflow", implementation.id);
            UINT64 total = subtotal + implementation.synchronization_cost;
            if (total < subtotal ||
                total + implementation.launch_cost < total)
                return DSL_Physical_IR_Report
                           (diagnostic, "cost overflow", implementation.id);
            total += implementation.launch_cost;
            if (implementation.id != expected_implementation ||
                implementation.site_id != site.id ||
                implementation.implementation_kind <=
                    DSL_PHYSICAL_IMPLEMENTATION_UNKNOWN ||
                implementation.implementation_kind >
                    DSL_PHYSICAL_IMPLEMENTATION_EXISTING_KERNEL ||
                implementation.provider <= DSL_PHYSICAL_PROVIDER_UNKNOWN ||
                implementation.provider >= DSL_PHYSICAL_PROVIDER_COUNT ||
                implementation.schedule_kind >
                    DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED ||
                implementation.identity == 0 ||
                implementation.total_cost != total ||
                implementation.legality < DSL_OPT_LEGALITY_PROVEN ||
                implementation.legality > DSL_OPT_LEGALITY_REJECTED ||
                implementation.rejection_reason >
                    DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
                (implementation.legality == DSL_OPT_LEGALITY_PROVEN &&
                 implementation.rejection_reason != DSL_OPT_REJECT_NONE) ||
                (implementation.legality == DSL_OPT_LEGALITY_REJECTED &&
                 implementation.rejection_reason == DSL_OPT_REJECT_NONE) ||
                implementation.candidate_id == DSL_OPT_CANDIDATE_INVALID_ID ||
                implementation.optimization_plan_id ==
                    DSL_OPT_PLAN_INVALID_ID ||
                implementation.reserved != 0 ||
                (implementation.flags &
                 ~(DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE |
                   DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVISIONAL |
                   DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED |
                   DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE |
                   DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING)) != 0 ||
                baseline !=
                    ((implementation.flags &
                      DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0) ||
                selected !=
                    ((implementation.flags &
                      DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED) != 0) ||
                (implementation.capability_id == 0 &&
                 (implementation.legality != DSL_OPT_LEGALITY_REJECTED ||
                  implementation.rejection_reason !=
                      DSL_OPT_REJECT_PROVIDER_MISMATCH ||
                  implementation.schedule_kind !=
                      DSL_PHYSICAL_SCHEDULE_UNKNOWN)) ||
                (implementation.capability_id != 0 &&
                 (!DSL_provider_capability_get
                      (implementation.capability_id, &capability) ||
                  implementation.schedule_kind ==
                      DSL_PHYSICAL_SCHEDULE_UNKNOWN ||
                  capability.provider != implementation.provider ||
                  capability.implementation_kind !=
                      implementation.implementation_kind ||
                  capability.schedule_kind != implementation.schedule_kind ||
                  (capability.target_profile_id != DSL_TARGET_PROFILE_UNKNOWN &&
                   capability.target_profile_id !=
                      implementation.target_profile_id))) ||
                (baseline &&
                 (implementation.provider !=
                      DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT ||
                  implementation.fallback_implementation_id != 0)) ||
                (!baseline &&
                 implementation.fallback_implementation_id !=
                    site.baseline_implementation_id))
                return DSL_Physical_IR_Report
                           (diagnostic, "invalid implementation",
                            implementation.id);
            if (selected) {
                ++selected_count;
                if (implementation.optimization_plan_id !=
                        site.selected_plan_id ||
                    implementation.legality != DSL_OPT_LEGALITY_PROVEN)
                    return DSL_Physical_IR_Report
                               (diagnostic, "invalid selection",
                                implementation.id);
            }
            ++expected_implementation;
        }
        if ((site.selected_implementation_id == 0 && selected_count != 0) ||
            (site.selected_implementation_id != 0 && selected_count != 1))
            return DSL_Physical_IR_Report
                       (diagnostic, "site selection is incomplete", site.id);
    }
    if (expected_implementation != ir->implementations.size() + 1)
        return DSL_Physical_IR_Report
                   (diagnostic, "unowned implementation", 0);
    return TRUE;
}

void
DSL_physical_plan_ir_print (FILE *file, const DSL_PHYSICAL_PLAN_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file,
            "CommonPhysicalPlanIR: owner=0x%x sites=%u implementations=%u\n",
            ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->implementations.size());
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_PHYSICAL_SITE_RECORD &site = ir->sites[i];
        fprintf(file,
                "  physical-site id=%u node=%u value=%u tile=%u fetch=%u "
                "implementations=%u baseline=%u selected=%u plan=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.selected_tile_plan_id, site.selected_fetch_plan_id,
                site.implementation_count, site.baseline_implementation_id,
                site.selected_implementation_id, site.selected_plan_id);
        for (UINT32 j = 0; j < site.implementation_count; ++j) {
            const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation =
                ir->implementations[site.first_implementation_id - 1 + j];
            fprintf(file,
                    "    physical-implementation id=%u identity=0x%llx "
                    "kind=%s provider=%s capability=%u schedule=%s "
                    "tile=%u fetch=%u legality=%s reason=%s cost=%llu "
                    "fallback=%u state=%s\n",
                    implementation.id,
                    (unsigned long long)implementation.identity,
                    DSL_physical_implementation_name
                        (implementation.implementation_kind),
                    DSL_physical_provider_name(implementation.provider),
                    implementation.capability_id,
                    DSL_physical_schedule_name(implementation.schedule_kind),
                    implementation.tile_plan_id,
                    implementation.fetch_plan_id,
                    DSL_opt_legality_name(implementation.legality),
                    DSL_opt_rejection_reason_name
                        (implementation.rejection_reason),
                    (unsigned long long)implementation.total_cost,
                    implementation.fallback_implementation_id,
                    (implementation.flags &
                     DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED) != 0 ?
                    "selected" : "not_selected");
        }
    }
}

ST_IDX
DSL_physical_plan_ir_owner (const DSL_PHYSICAL_PLAN_IR *ir)
{
    return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st;
}

UINT32
DSL_physical_plan_ir_site_count (const DSL_PHYSICAL_PLAN_IR *ir)
{
    return ir == NULL ? 0 : ir->sites.size();
}

UINT32
DSL_physical_plan_ir_implementation_count (const DSL_PHYSICAL_PLAN_IR *ir)
{
    return ir == NULL ? 0 : ir->implementations.size();
}

BOOL
DSL_physical_plan_ir_get_site
        (const DSL_PHYSICAL_PLAN_IR *ir, DSL_PHYSICAL_SITE_ID id,
         DSL_PHYSICAL_SITE_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->sites.size())
        return FALSE;
    *record = ir->sites[id - 1];
    return TRUE;
}

BOOL
DSL_physical_plan_ir_get_implementation
        (const DSL_PHYSICAL_PLAN_IR *ir, DSL_PHYSICAL_IMPLEMENTATION_ID id,
         DSL_PHYSICAL_IMPLEMENTATION_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 ||
        id > ir->implementations.size())
        return FALSE;
    *record = ir->implementations[id - 1];
    return TRUE;
}

BOOL
DSL_physical_plan_ir_find_selected
        (const DSL_PHYSICAL_PLAN_IR *ir, DSL_IR_NODE_ID semantic_node_id,
         DSL_PHYSICAL_SITE_RECORD *site,
         DSL_PHYSICAL_IMPLEMENTATION_RECORD *implementation)
{
    if (ir == NULL || semantic_node_id == DSL_IR_NODE_INVALID_ID ||
        site == NULL || implementation == NULL)
        return FALSE;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_PHYSICAL_SITE_RECORD &candidate = ir->sites[i];
        if (candidate.semantic_node_id != semantic_node_id)
            continue;
        *site = candidate;
        return DSL_physical_plan_ir_get_implementation
                   (ir, candidate.selected_implementation_id,
                    implementation);
    }
    return FALSE;
}
