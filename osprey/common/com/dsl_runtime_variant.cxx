/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * RuntimeVariantIR storage, construction, structural verification, access,
 * and generic printing. VHO owns fact capture and optimization policy.
 */

#include <string.h>
#include <vector>

#include "dsl_runtime_variant.h"

struct DSL_RUNTIME_VARIANT_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_RUNTIME_VARIANT_SITE_RECORD> sites;
    std::vector<DSL_RUNTIME_VARIANT_RECORD> variants;
    std::vector<DSL_RUNTIME_GUARD_RECORD> guards;
};

static const char *DSL_runtime_guard_kind_name_table[] = {
    "unknown", "operand_alignment", "shape_dimension_equal",
    "shape_dimension_multiple"
};

static const char *DSL_runtime_guard_comparison_name_table[] = {
    "unknown", "at_least", "equal", "multiple_of"
};

static BOOL
DSL_Runtime_IR_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "RuntimeVariantIR error: %s id=%u\n",
                message, id);
    return FALSE;
}

const char *
DSL_runtime_guard_kind_name (UINT32 kind)
{
    return kind < sizeof(DSL_runtime_guard_kind_name_table) /
                      sizeof(DSL_runtime_guard_kind_name_table[0]) ?
           DSL_runtime_guard_kind_name_table[kind] : "unknown";
}

const char *
DSL_runtime_guard_comparison_name (UINT32 comparison)
{
    return comparison < sizeof(DSL_runtime_guard_comparison_name_table) /
                            sizeof(DSL_runtime_guard_comparison_name_table[0]) ?
           DSL_runtime_guard_comparison_name_table[comparison] : "unknown";
}

BOOL
DSL_runtime_variant_ir_verify
        (const DSL_RUNTIME_VARIANT_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Runtime_IR_Report(diagnostic, "invalid owner", 0);

    UINT32 expected_variant = 1;
    UINT32 expected_guard = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_RUNTIME_VARIANT_SITE_RECORD &site = ir->sites[i];
        UINT32 selected_count = 0;
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_node_id == DSL_IR_NODE_INVALID_ID ||
            site.semantic_value_id == DSL_IR_VALUE_INVALID_ID ||
            site.physical_site_id == 0 ||
            site.first_variant_id != expected_variant ||
            site.variant_count == 0 ||
            site.baseline_variant_id < site.first_variant_id ||
            site.baseline_variant_id >=
                site.first_variant_id + site.variant_count ||
            (site.selected_variant_id != 0 &&
             (site.selected_variant_id < site.first_variant_id ||
              site.selected_variant_id >=
                  site.first_variant_id + site.variant_count)) ||
            site.selection_policy != DSL_RUNTIME_SELECTION_FIRST_MATCH ||
            site.reserved != 0)
            return DSL_Runtime_IR_Report
                       (diagnostic, "invalid site", site.id);

        for (UINT32 j = 0; j < site.variant_count; ++j) {
            if (expected_variant > ir->variants.size())
                return DSL_Runtime_IR_Report
                           (diagnostic, "missing variant", expected_variant);
            const DSL_RUNTIME_VARIANT_RECORD &variant =
                ir->variants[expected_variant - 1];
            if (variant.id != expected_variant ||
                variant.site_id != site.id ||
                variant.physical_implementation_id == 0 ||
                variant.candidate_id == DSL_OPT_CANDIDATE_INVALID_ID ||
                variant.optimization_plan_id == DSL_OPT_PLAN_INVALID_ID ||
                ~(UINT64)0 - variant.physical_cost < variant.guard_cost ||
                variant.total_cost !=
                    variant.physical_cost + variant.guard_cost ||
                variant.reserved != 0 ||
                (variant.flags &
                 ~(DSL_RUNTIME_VARIANT_FLAG_BASELINE |
                   DSL_RUNTIME_VARIANT_FLAG_GUARDED |
                   DSL_RUNTIME_VARIANT_FLAG_CERTIFIED |
                   DSL_RUNTIME_VARIANT_FLAG_SELECTED)) != 0 ||
                (variant.flags & DSL_RUNTIME_VARIANT_FLAG_CERTIFIED) == 0 ||
                ((variant.flags & DSL_RUNTIME_VARIANT_FLAG_BASELINE) != 0 &&
                 (variant.flags & DSL_RUNTIME_VARIANT_FLAG_GUARDED) != 0) ||
                ((variant.flags & DSL_RUNTIME_VARIANT_FLAG_SELECTED) != 0) !=
                    (variant.id == site.selected_variant_id) ||
                (variant.fallback_variant_id != 0 &&
                 (variant.fallback_variant_id == variant.id ||
                  variant.fallback_variant_id < site.first_variant_id ||
                  variant.fallback_variant_id >=
                      site.first_variant_id + site.variant_count)))
                return DSL_Runtime_IR_Report
                           (diagnostic, "invalid variant", variant.id);
            if ((variant.flags & DSL_RUNTIME_VARIANT_FLAG_SELECTED) != 0)
                ++selected_count;
            if (variant.guard_count == 0) {
                if (variant.first_guard_id != 0 || variant.guard_cost != 0)
                    return DSL_Runtime_IR_Report
                               (diagnostic, "invalid guardless variant",
                                variant.id);
            } else if (variant.first_guard_id != expected_guard) {
                return DSL_Runtime_IR_Report
                           (diagnostic, "invalid guard range", variant.id);
            }
            UINT64 guard_cost = 0;
            for (UINT32 k = 0; k < variant.guard_count; ++k) {
                if (expected_guard > ir->guards.size())
                    return DSL_Runtime_IR_Report
                               (diagnostic, "missing guard", expected_guard);
                const DSL_RUNTIME_GUARD_RECORD &guard =
                    ir->guards[expected_guard - 1];
                if (guard.id != expected_guard ||
                    guard.variant_id != variant.id ||
                    guard.kind <= DSL_RUNTIME_GUARD_UNKNOWN ||
                    guard.kind >
                        DSL_RUNTIME_GUARD_SHAPE_DIMENSION_MULTIPLE ||
                    guard.comparison <=
                        DSL_RUNTIME_GUARD_COMPARE_UNKNOWN ||
                    guard.comparison >
                        DSL_RUNTIME_GUARD_COMPARE_MULTIPLE_OF ||
                    guard.failure_action !=
                        DSL_RUNTIME_GUARD_FAILURE_FALLBACK ||
                    guard.required_value == 0 ||
                    guard.evaluation_cost == 0 ||
                    guard.flags != DSL_RUNTIME_GUARD_FLAG_REQUIRED ||
                    guard.reserved0 != 0 || guard.reserved1 != 0 ||
                    ~(UINT64)0 - guard_cost < guard.evaluation_cost)
                    return DSL_Runtime_IR_Report
                               (diagnostic, "invalid guard", guard.id);
                guard_cost += guard.evaluation_cost;
                ++expected_guard;
            }
            if (guard_cost != variant.guard_cost)
                return DSL_Runtime_IR_Report
                           (diagnostic, "guard cost mismatch", variant.id);
            ++expected_variant;
        }
        if ((site.selected_variant_id == 0 && selected_count != 0) ||
            (site.selected_variant_id != 0 && selected_count != 1))
            return DSL_Runtime_IR_Report
                       (diagnostic, "invalid selected variant count", site.id);
    }
    if (expected_variant != ir->variants.size() + 1 ||
        expected_guard != ir->guards.size() + 1)
        return DSL_Runtime_IR_Report(diagnostic, "orphan record", 0);
    return TRUE;
}

DSL_RUNTIME_VARIANT_IR *
DSL_runtime_variant_ir_create
        (const DSL_RUNTIME_VARIANT_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->variant_count != 0 && info->variants == NULL) ||
        (info->guard_count != 0 && info->guards == NULL)) {
        DSL_Runtime_IR_Report(diagnostic, "invalid create request", 0);
        return NULL;
    }
    DSL_RUNTIME_VARIANT_IR *ir = new DSL_RUNTIME_VARIANT_IR;
    ir->owner_pu_st = info->owner_pu_st;
    if (info->site_count != 0)
        ir->sites.assign(info->sites, info->sites + info->site_count);
    if (info->variant_count != 0)
        ir->variants.assign(info->variants,
                            info->variants + info->variant_count);
    if (info->guard_count != 0)
        ir->guards.assign(info->guards, info->guards + info->guard_count);
    if (!DSL_runtime_variant_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void
DSL_runtime_variant_ir_destroy (DSL_RUNTIME_VARIANT_IR *ir)
{
    delete ir;
}

void
DSL_runtime_variant_ir_print (FILE *file, const DSL_RUNTIME_VARIANT_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file,
            "RuntimeVariantIR: owner=0x%x sites=%u variants=%u guards=%u\n",
            ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->variants.size(), (UINT32)ir->guards.size());
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_RUNTIME_VARIANT_SITE_RECORD &site = ir->sites[i];
        fprintf(file,
                "  site id=%u node=%u value=%u physical=%u variants=%u "
                "baseline=%u selected=%u policy=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.physical_site_id, site.variant_count,
                site.baseline_variant_id, site.selected_variant_id,
                site.selection_policy);
        for (UINT32 j = 0; j < site.variant_count; ++j) {
            const DSL_RUNTIME_VARIANT_RECORD &variant =
                ir->variants[site.first_variant_id - 1 + j];
            fprintf(file,
                    "    variant id=%u implementation=%u guards=%u "
                    "fallback=%u candidate=%u plan=%u total=%llu "
                    "flags=0x%x\n",
                    variant.id, variant.physical_implementation_id,
                    variant.guard_count, variant.fallback_variant_id,
                    variant.candidate_id, variant.optimization_plan_id,
                    (unsigned long long)variant.total_cost, variant.flags);
        }
    }
}

ST_IDX
DSL_runtime_variant_ir_owner (const DSL_RUNTIME_VARIANT_IR *ir)
{
    return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st;
}

UINT32
DSL_runtime_variant_ir_site_count (const DSL_RUNTIME_VARIANT_IR *ir)
{
    return ir == NULL ? 0 : ir->sites.size();
}

UINT32
DSL_runtime_variant_ir_variant_count (const DSL_RUNTIME_VARIANT_IR *ir)
{
    return ir == NULL ? 0 : ir->variants.size();
}

UINT32
DSL_runtime_variant_ir_guard_count (const DSL_RUNTIME_VARIANT_IR *ir)
{
    return ir == NULL ? 0 : ir->guards.size();
}

BOOL
DSL_runtime_variant_ir_get_site
        (const DSL_RUNTIME_VARIANT_IR *ir, DSL_RUNTIME_VARIANT_SITE_ID id,
         DSL_RUNTIME_VARIANT_SITE_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->sites.size())
        return FALSE;
    *record = ir->sites[id - 1];
    return TRUE;
}

BOOL
DSL_runtime_variant_ir_get_variant
        (const DSL_RUNTIME_VARIANT_IR *ir, DSL_RUNTIME_VARIANT_ID id,
         DSL_RUNTIME_VARIANT_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 ||
        id > ir->variants.size())
        return FALSE;
    *record = ir->variants[id - 1];
    return TRUE;
}

BOOL
DSL_runtime_variant_ir_get_guard
        (const DSL_RUNTIME_VARIANT_IR *ir, DSL_RUNTIME_GUARD_ID id,
         DSL_RUNTIME_GUARD_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->guards.size())
        return FALSE;
    *record = ir->guards[id - 1];
    return TRUE;
}
