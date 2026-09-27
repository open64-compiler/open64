/*
 * Copyright (C) 2026 Open64 Project
 */

/* Policy-free AIO-7 CommonDistributedPlanIR storage and structural services. */

#include <vector>

#include "dsl_distributed_candidate.h"

struct DSL_DISTRIBUTED_PLAN_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_DISTRIBUTED_DESCRIPTOR_RECORD> descriptors;
    std::vector<DSL_DISTRIBUTED_ALIAS_RECORD> aliases;
    std::vector<DSL_DISTRIBUTED_RANGE_RECORD> ranges;
    std::vector<DSL_DISTRIBUTED_SITE_RECORD> sites;
    std::vector<DSL_DISTRIBUTED_ALTERNATIVE_RECORD> alternatives;
    std::vector<DSL_COMMUNICATION_EPOCH_RECORD> epochs;
    std::vector<DSL_COMMUNICATION_INTENT_RECORD> intents;
};

static const char *DSL_placement_kind_name_table[] = {
    "unknown", "replicated", "partitioned", "remote_single"
};
static const char *DSL_sharding_kind_name_table[] = {
    "unknown", "replicated", "axis", "partial_reduction", "migrated"
};
static const char *DSL_distributed_ownership_name_table[] = {
    "unknown", "disjoint", "replicated", "reduced", "migrated"
};
static const char *DSL_distributed_range_state_name_table[] = {
    "unknown", "exact"
};
static const char *DSL_distributed_disjoint_state_name_table[] = {
    "unknown", "proven", "overlap"
};
static const char *DSL_communication_kind_name_table[] = {
    "unknown", "all_gather", "scatter", "all_reduce", "reduce_scatter",
    "all_to_all", "peer_copy"
};

static BOOL
DSL_Distributed_IR_Report (FILE *file, const char *message, UINT32 id)
{
    if (file != NULL)
        fprintf(file, "DSL distributed IR error: %s id=%u\n", message, id);
    return FALSE;
}

#define DSL_DISTRIBUTED_NAME(function, table)                            \
const char *function (UINT32 value)                                      \
{                                                                        \
    return value < sizeof(table) / sizeof(table[0]) ? table[value] :      \
           "unknown";                                                    \
}
DSL_DISTRIBUTED_NAME(DSL_placement_kind_name,
                     DSL_placement_kind_name_table)
DSL_DISTRIBUTED_NAME(DSL_sharding_kind_name,
                     DSL_sharding_kind_name_table)
DSL_DISTRIBUTED_NAME(DSL_distributed_ownership_name,
                     DSL_distributed_ownership_name_table)
DSL_DISTRIBUTED_NAME(DSL_distributed_range_state_name,
                     DSL_distributed_range_state_name_table)
DSL_DISTRIBUTED_NAME(DSL_distributed_disjoint_state_name,
                     DSL_distributed_disjoint_state_name_table)
DSL_DISTRIBUTED_NAME(DSL_communication_kind_name,
                     DSL_communication_kind_name_table)
#undef DSL_DISTRIBUTED_NAME

BOOL
DSL_distributed_plan_ir_verify
        (const DSL_DISTRIBUTED_PLAN_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Distributed_IR_Report(diagnostic, "invalid owner", 0);
    if (ir->descriptors.size() != ir->aliases.size())
        return DSL_Distributed_IR_Report
                   (diagnostic, "descriptor/alias count mismatch", 0);
    UINT32 range_id = 1;
    for (UINT32 i = 0; i < ir->descriptors.size(); ++i) {
        const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor =
            ir->descriptors[i];
        const DSL_DISTRIBUTED_ALIAS_RECORD &alias = ir->aliases[i];
        if (descriptor.id != i + 1 || alias.id != i + 1 ||
            descriptor.alias_id != alias.id ||
            alias.descriptor_id != descriptor.id ||
            descriptor.source_descriptor_ty == TY_IDX_ZERO ||
            descriptor.placement_kind <= DSL_PLACEMENT_UNKNOWN ||
            descriptor.placement_kind > DSL_PLACEMENT_REMOTE_SINGLE ||
            descriptor.sharding_kind <= DSL_SHARDING_UNKNOWN ||
            descriptor.sharding_kind > DSL_SHARDING_MIGRATED ||
            descriptor.ownership > DSL_DISTRIBUTED_OWNERSHIP_MIGRATED ||
            descriptor.device_count < 2 ||
            descriptor.flags & ~(DSL_DISTRIBUTED_FLAG_PROVISIONAL |
                                 DSL_DISTRIBUTED_FLAG_SEMANTICS_PRESERVING) ||
            descriptor.reserved != 0 ||
            alias.first_range_id != range_id || alias.range_count == 0 ||
            alias.ownership != descriptor.ownership ||
            alias.disjoint_state > DSL_DISTRIBUTED_DISJOINT_OVERLAP ||
            alias.reserved != 0)
            return DSL_Distributed_IR_Report
                       (diagnostic, "invalid descriptor", descriptor.id);
        for (UINT32 j = 0; j < alias.range_count; ++j, ++range_id) {
            if (range_id > ir->ranges.size())
                return DSL_Distributed_IR_Report
                           (diagnostic, "range overflow", alias.id);
            const DSL_DISTRIBUTED_RANGE_RECORD &range =
                ir->ranges[range_id - 1];
            if (range.id != range_id || range.alias_id != alias.id ||
                range.device_ordinal >= descriptor.device_count ||
                range.state > DSL_DISTRIBUTED_RANGE_EXACT ||
                (range.state == DSL_DISTRIBUTED_RANGE_EXACT &&
                 (range.lower == DSL_DISTRIBUTED_UNKNOWN_U64 ||
                  range.upper == DSL_DISTRIBUTED_UNKNOWN_U64 ||
                  range.lower >= range.upper)) ||
                (range.state == DSL_DISTRIBUTED_RANGE_UNKNOWN &&
                 (range.lower != DSL_DISTRIBUTED_UNKNOWN_U64 ||
                  range.upper != DSL_DISTRIBUTED_UNKNOWN_U64)) ||
                range.reserved != 0)
                return DSL_Distributed_IR_Report
                           (diagnostic, "invalid range", range.id);
        }
    }
    if (range_id != ir->ranges.size() + 1)
        return DSL_Distributed_IR_Report(diagnostic, "orphan range", 0);

    UINT32 alternative_id = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_DISTRIBUTED_SITE_RECORD &site = ir->sites[i];
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_value_id == 0 || site.semantic_root_id == 0 ||
            site.first_alternative_id != alternative_id ||
            site.alternative_count == 0 || site.epoch_id == 0 ||
            site.epoch_id > ir->epochs.size() ||
            site.baseline_candidate_id == 0 || site.baseline_plan_id == 0 ||
            site.reserved != 0)
            return DSL_Distributed_IR_Report
                       (diagnostic, "invalid site", site.id);
        const DSL_COMMUNICATION_EPOCH_RECORD &epoch =
            ir->epochs[site.epoch_id - 1];
        if (epoch.id != site.epoch_id || epoch.site_id != site.id ||
            epoch.semantic_value_id != site.semantic_value_id ||
            epoch.producer_node_id == 0 || epoch.consumer_count == 0 ||
            epoch.reserved != 0)
            return DSL_Distributed_IR_Report
                       (diagnostic, "invalid epoch", epoch.id);
        BOOL selected_found = site.selected_plan_id == 0 ||
                              site.selected_plan_id == site.baseline_plan_id;
        for (UINT32 j = 0; j < site.alternative_count;
             ++j, ++alternative_id) {
            if (alternative_id > ir->alternatives.size())
                return DSL_Distributed_IR_Report
                           (diagnostic, "alternative overflow", site.id);
            const DSL_DISTRIBUTED_ALTERNATIVE_RECORD &alternative =
                ir->alternatives[alternative_id - 1];
            if (alternative.id != alternative_id ||
                alternative.site_id != site.id ||
                alternative.descriptor_id == 0 ||
                alternative.descriptor_id > ir->descriptors.size() ||
                alternative.result_evolution_node_id == 0 ||
                alternative.evolution_edge_id == 0 ||
                alternative.legality > DSL_OPT_LEGALITY_REJECTED ||
                alternative.rejection_reason >
                    DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
                alternative.candidate_id == 0 || alternative.plan_id == 0 ||
                alternative.reserved != 0)
                return DSL_Distributed_IR_Report
                           (diagnostic, "invalid alternative", alternative.id);
            if (alternative.communication_intent_id != 0) {
                if (alternative.communication_intent_id >
                        ir->intents.size())
                    return DSL_Distributed_IR_Report
                               (diagnostic, "missing intent", alternative.id);
                const DSL_COMMUNICATION_INTENT_RECORD &intent =
                    ir->intents[alternative.communication_intent_id - 1];
                if (intent.id != alternative.communication_intent_id ||
                    intent.alternative_id != alternative.id ||
                    intent.epoch_id != site.epoch_id ||
                    intent.kind <= DSL_COMMUNICATION_UNKNOWN ||
                    intent.kind > DSL_COMMUNICATION_PEER_COPY ||
                    intent.source_count == 0 ||
                    intent.destination_count == 0 ||
                    intent.legality != alternative.legality ||
                    intent.rejection_reason !=
                        alternative.rejection_reason ||
                    intent.reserved0 != 0 || intent.reserved1 != 0)
                    return DSL_Distributed_IR_Report
                               (diagnostic, "invalid intent", intent.id);
            }
            if (site.selected_plan_id == alternative.plan_id)
                selected_found = TRUE;
        }
        if (!selected_found)
            return DSL_Distributed_IR_Report
                       (diagnostic, "selected plan is absent", site.id);
    }
    if (alternative_id != ir->alternatives.size() + 1 ||
        ir->epochs.size() != ir->sites.size())
        return DSL_Distributed_IR_Report
                   (diagnostic, "table range mismatch", 0);
    for (UINT32 i = 0; i < ir->intents.size(); ++i)
        if (ir->intents[i].id != i + 1)
            return DSL_Distributed_IR_Report
                       (diagnostic, "invalid intent id", i + 1);
    return TRUE;
}

DSL_DISTRIBUTED_PLAN_IR *
DSL_distributed_plan_ir_create
        (const DSL_DISTRIBUTED_PLAN_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->descriptor_count != 0 && info->descriptors == NULL) ||
        (info->alias_count != 0 && info->aliases == NULL) ||
        (info->range_count != 0 && info->ranges == NULL) ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->alternative_count != 0 && info->alternatives == NULL) ||
        (info->epoch_count != 0 && info->epochs == NULL) ||
        (info->intent_count != 0 && info->intents == NULL)) {
        DSL_Distributed_IR_Report(diagnostic, "invalid create info", 0);
        return NULL;
    }
    DSL_DISTRIBUTED_PLAN_IR *ir = new DSL_DISTRIBUTED_PLAN_IR;
    ir->owner_pu_st = info->owner_pu_st;
#define DSL_DISTRIBUTED_COPY(field, count)                               \
    if (info->count != 0)                                                \
        ir->field.assign(info->field, info->field + info->count)
    DSL_DISTRIBUTED_COPY(descriptors, descriptor_count);
    DSL_DISTRIBUTED_COPY(aliases, alias_count);
    DSL_DISTRIBUTED_COPY(ranges, range_count);
    DSL_DISTRIBUTED_COPY(sites, site_count);
    DSL_DISTRIBUTED_COPY(alternatives, alternative_count);
    DSL_DISTRIBUTED_COPY(epochs, epoch_count);
    DSL_DISTRIBUTED_COPY(intents, intent_count);
#undef DSL_DISTRIBUTED_COPY
    if (!DSL_distributed_plan_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void DSL_distributed_plan_ir_destroy(DSL_DISTRIBUTED_PLAN_IR *ir)
{ delete ir; }

void
DSL_distributed_plan_ir_print (FILE *file, const DSL_DISTRIBUTED_PLAN_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file,
            "CommonDistributedPlanIR: owner=0x%x sites=%u descriptors=%u "
            "ranges=%u alternatives=%u epochs=%u intents=%u\n",
            ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->descriptors.size(), (UINT32)ir->ranges.size(),
            (UINT32)ir->alternatives.size(), (UINT32)ir->epochs.size(),
            (UINT32)ir->intents.size());
}

ST_IDX DSL_distributed_plan_ir_owner(const DSL_DISTRIBUTED_PLAN_IR *ir)
{ return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st; }
#define DSL_DISTRIBUTED_COUNT(name, field)                               \
UINT32 DSL_distributed_plan_ir_##name##_count                            \
        (const DSL_DISTRIBUTED_PLAN_IR *ir)                              \
{ return ir == NULL ? 0 : ir->field.size(); }
DSL_DISTRIBUTED_COUNT(descriptor, descriptors)
DSL_DISTRIBUTED_COUNT(alias, aliases)
DSL_DISTRIBUTED_COUNT(range, ranges)
DSL_DISTRIBUTED_COUNT(site, sites)
DSL_DISTRIBUTED_COUNT(alternative, alternatives)
DSL_DISTRIBUTED_COUNT(epoch, epochs)
DSL_DISTRIBUTED_COUNT(intent, intents)
#undef DSL_DISTRIBUTED_COUNT

#define DSL_DISTRIBUTED_GET(name, type, record_type, field)              \
BOOL DSL_distributed_plan_ir_get_##name                                  \
        (const DSL_DISTRIBUTED_PLAN_IR *ir, type id, record_type *record) \
{                                                                        \
    if (ir == NULL || record == NULL || id == 0 || id > ir->field.size()) \
        return FALSE;                                                     \
    *record = ir->field[id - 1];                                         \
    return TRUE;                                                         \
}
DSL_DISTRIBUTED_GET(descriptor, DSL_DISTRIBUTED_DESCRIPTOR_ID,
                    DSL_DISTRIBUTED_DESCRIPTOR_RECORD, descriptors)
DSL_DISTRIBUTED_GET(alias, DSL_DISTRIBUTED_ALIAS_ID,
                    DSL_DISTRIBUTED_ALIAS_RECORD, aliases)
DSL_DISTRIBUTED_GET(range, DSL_DISTRIBUTED_RANGE_ID,
                    DSL_DISTRIBUTED_RANGE_RECORD, ranges)
DSL_DISTRIBUTED_GET(site, DSL_DISTRIBUTED_SITE_ID,
                    DSL_DISTRIBUTED_SITE_RECORD, sites)
DSL_DISTRIBUTED_GET(alternative, DSL_DISTRIBUTED_ALTERNATIVE_ID,
                    DSL_DISTRIBUTED_ALTERNATIVE_RECORD, alternatives)
DSL_DISTRIBUTED_GET(epoch, DSL_COMMUNICATION_EPOCH_ID,
                    DSL_COMMUNICATION_EPOCH_RECORD, epochs)
DSL_DISTRIBUTED_GET(intent, DSL_COMMUNICATION_INTENT_ID,
                    DSL_COMMUNICATION_INTENT_RECORD, intents)
#undef DSL_DISTRIBUTED_GET
