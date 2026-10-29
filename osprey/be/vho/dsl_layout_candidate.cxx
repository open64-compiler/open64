/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned copied storage, structural validation and runtime inspection for
 * CommonLogicalLayoutIR. The caller controls PU scope and store lifetime;
 * this service does not inspect active WHIRL or make optimization decisions.
 * Relocation preserves executable behavior and adds no .B rows, sections or
 * reader/writer changes. Policy remains in the companion *_opt implementation.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#include <vector>

#include "dsl_layout_candidate.h"

struct DSL_LOGICAL_LAYOUT_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD> descriptors;
    std::vector<DSL_LOGICAL_LAYOUT_AXIS_RECORD> axes;
    std::vector<DSL_LOGICAL_LAYOUT_BLOCK_RECORD> blocks;
    std::vector<DSL_LOGICAL_LAYOUT_SITE_RECORD> sites;
    std::vector<DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD> alternatives;
};

static const char *DSL_logical_layout_kind_name_table[] = {
    "unknown", "permuted", "blocked", "packed_head", "domain"
};
static const char *DSL_layout_compatibility_name_table[] = {
    "unknown", "proven", "rejected"
};
static const char *DSL_layout_conversion_name_table[] = {
    "unknown", "known"
};

/* Return FALSE for the caller's rejected structural check, optionally writing
 * message and a runtime table ID to the borrowed stream. ID zero denotes an
 * owner/create/table-wide failure; no record, output buffer or IR is mutated. */
static BOOL
DSL_Layout_IR_Report (FILE *file, const char *message, UINT32 id)
{
    if (file != NULL)
        fprintf(file, "DSL logical layout IR error: %s id=%u\n", message, id);
    return FALSE;
}

/* Borrow a static logical layout kind spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
const char *DSL_logical_layout_kind_name(UINT32 value)
{ return value < 5 ? DSL_logical_layout_kind_name_table[value] : "unknown"; }
/* Borrow a static compatibility state spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
const char *DSL_layout_compatibility_name(UINT32 value)
{ return value < 3 ? DSL_layout_compatibility_name_table[value] : "unknown"; }
/* Borrow a static conversion state spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
const char *DSL_layout_conversion_name(UINT32 value)
{ return value < 2 ? DSL_layout_conversion_name_table[value] : "unknown"; }

/* Read-only structural check of owner, dense descriptor/detail ranges,
 * permutations, distinct blocked axes, alternative joins and selected-plan
 * membership. This does not resolve TY/evolution IDs or prove conversions legal.
 * Return FALSE on a defect, usually with a diagnostic; the final orphan-
 * alternative check can fail silently. */
BOOL
DSL_logical_layout_ir_verify
        (const DSL_LOGICAL_LAYOUT_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Layout_IR_Report(diagnostic, "invalid owner", 0);
    /* Descriptor detail ranges partition the copied axis/block tables;
     * source-axis uniqueness gives a permutation, not conversion legality. */
    UINT32 axis_id = 1;
    UINT32 block_id = 1;
    for (UINT32 i = 0; i < ir->descriptors.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &descriptor =
            ir->descriptors[i];
        if (descriptor.id != i + 1 ||
            descriptor.source_descriptor_ty == TY_IDX_ZERO ||
            descriptor.kind < DSL_LOGICAL_LAYOUT_PERMUTED ||
            descriptor.kind > DSL_LOGICAL_LAYOUT_DOMAIN ||
            descriptor.rank == 0 || descriptor.first_axis_id != axis_id ||
            descriptor.axis_count != descriptor.rank ||
            (descriptor.block_count == 0) !=
                (descriptor.first_block_id == 0) ||
            (descriptor.block_count != 0 &&
             descriptor.first_block_id != block_id) ||
            (descriptor.kind == DSL_LOGICAL_LAYOUT_BLOCKED) !=
                (descriptor.block_count != 0) ||
            descriptor.minimum_alignment == 0 ||
            descriptor.flags !=
                (DSL_LOGICAL_LAYOUT_FLAG_SEMANTICS_PRESERVING |
                 DSL_LOGICAL_LAYOUT_FLAG_LOGICAL_ONLY) ||
            descriptor.reserved != 0)
            return DSL_Layout_IR_Report
                       (diagnostic, "invalid descriptor", descriptor.id);
        std::vector<BOOL> seen(descriptor.rank, FALSE);
        for (UINT32 j = 0; j < descriptor.axis_count; ++j, ++axis_id) {
            if (axis_id > ir->axes.size())
                return DSL_Layout_IR_Report
                           (diagnostic, "axis range overflow", descriptor.id);
            const DSL_LOGICAL_LAYOUT_AXIS_RECORD &axis = ir->axes[axis_id - 1];
            if (axis.id != axis_id || axis.descriptor_id != descriptor.id ||
                axis.ordinal != j || axis.source_axis >= descriptor.rank ||
                axis.result_axis != j || seen[axis.source_axis] ||
                axis.reserved != 0)
                return DSL_Layout_IR_Report
                           (diagnostic, "invalid axis", axis.id);
            seen[axis.source_axis] = TRUE;
        }
        std::vector<BOOL> blocked(descriptor.rank, FALSE);
        for (UINT32 j = 0; j < descriptor.block_count; ++j, ++block_id) {
            if (block_id > ir->blocks.size())
                return DSL_Layout_IR_Report
                           (diagnostic, "block range overflow", descriptor.id);
            const DSL_LOGICAL_LAYOUT_BLOCK_RECORD &block =
                ir->blocks[block_id - 1];
            if (block.id != block_id ||
                block.descriptor_id != descriptor.id ||
                block.axis >= descriptor.rank || blocked[block.axis] ||
                block.factor <= 1 || block.reserved0 != 0 ||
                block.reserved1 != 0)
                return DSL_Layout_IR_Report
                           (diagnostic, "invalid block", block.id);
            blocked[block.axis] = TRUE;
        }
    }
    if (axis_id != ir->axes.size() + 1 || block_id != ir->blocks.size() + 1)
        return DSL_Layout_IR_Report(diagnostic, "orphan detail", 0);

    /* Site alternatives form a separate dense partition; selections must
     * name the baseline or a plan in that site's own alternative range. */
    UINT32 alternative_id = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_SITE_RECORD &site = ir->sites[i];
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_value_id == 0 || site.semantic_root_id == 0 ||
            site.first_alternative_id != alternative_id ||
            site.alternative_count == 0 || site.baseline_candidate_id == 0 ||
            site.baseline_plan_id == 0 || site.reserved != 0)
            return DSL_Layout_IR_Report(diagnostic, "invalid site", site.id);
        BOOL selected_found = site.selected_plan_id == 0 ||
                              site.selected_plan_id == site.baseline_plan_id;
        for (UINT32 j = 0; j < site.alternative_count;
             ++j, ++alternative_id) {
            if (alternative_id > ir->alternatives.size())
                return DSL_Layout_IR_Report
                           (diagnostic, "alternative range overflow", site.id);
            const DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD &alternative =
                ir->alternatives[alternative_id - 1];
            if (alternative.id != alternative_id ||
                alternative.site_id != site.id ||
                alternative.descriptor_id == 0 ||
                alternative.descriptor_id > ir->descriptors.size() ||
                alternative.result_evolution_node_id == 0 ||
                alternative.evolution_edge_id == 0 ||
                alternative.compatibility_state >
                    DSL_LAYOUT_COMPATIBILITY_REJECTED ||
                alternative.conversion_state > DSL_LAYOUT_CONVERSION_KNOWN ||
                alternative.legality > DSL_OPT_LEGALITY_REJECTED ||
                alternative.rejection_reason >
                    DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
                alternative.candidate_id == 0 || alternative.plan_id == 0 ||
                alternative.reserved != 0)
                return DSL_Layout_IR_Report
                           (diagnostic, "invalid alternative", alternative.id);
            if (site.selected_plan_id == alternative.plan_id)
                selected_found = TRUE;
        }
        if (!selected_found)
            return DSL_Layout_IR_Report
                       (diagnostic, "selected plan is absent", site.id);
    }
    return alternative_id == ir->alternatives.size() + 1;
}

/* Copy the caller's readable, count-sized record arrays into a new owned
 * runtime store for owner_pu_st; nonempty arrays must be nonnull. Return NULL
 * on invalid input or structural verification failure, deleting a rejected
 * copy without changing caller arrays. The caller destroys success; allocation
 * exceptions are not caught. */
DSL_LOGICAL_LAYOUT_IR *
DSL_logical_layout_ir_create
        (const DSL_LOGICAL_LAYOUT_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->descriptor_count != 0 && info->descriptors == NULL) ||
        (info->axis_count != 0 && info->axes == NULL) ||
        (info->block_count != 0 && info->blocks == NULL) ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->alternative_count != 0 && info->alternatives == NULL)) {
        DSL_Layout_IR_Report(diagnostic, "invalid create info", 0);
        return NULL;
    }
    DSL_LOGICAL_LAYOUT_IR *ir = new DSL_LOGICAL_LAYOUT_IR;
    ir->owner_pu_st = info->owner_pu_st;
/* Copy a nonempty, preflighted caller array into the unpublished store's
 * vector. Preserve row IDs and ordering; zero count avoids null pointer
 * arithmetic. Allocation exceptions propagate rather than returning a status. */
#define DSL_LAYOUT_COPY(field, count)                                    \
    if (info->count != 0)                                                \
        ir->field.assign(info->field, info->field + info->count)
    DSL_LAYOUT_COPY(descriptors, descriptor_count);
    DSL_LAYOUT_COPY(axes, axis_count);
    DSL_LAYOUT_COPY(blocks, block_count);
    DSL_LAYOUT_COPY(sites, site_count);
    DSL_LAYOUT_COPY(alternatives, alternative_count);
#undef DSL_LAYOUT_COPY
    if (!DSL_logical_layout_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

/* Release this caller-owned runtime store and its copied vectors; NULL is
 * safe. Other aliases become invalid; referenced WHIRL, types and plan IDs are
 * not owned or destroyed here. */
void DSL_logical_layout_ir_destroy(DSL_LOGICAL_LAYOUT_IR *ir) { delete ir; }

/* Print stored layout table counts and site summaries to the borrowed stream;
 * null stream or store is a no-op. Neither validate compatibility nor select
 * layouts, intern tensor types or change mapped-image state. */
void
DSL_logical_layout_ir_print (FILE *file, const DSL_LOGICAL_LAYOUT_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file,
            "CommonLogicalLayoutIR: owner=0x%x descriptors=%u axes=%u "
            "blocks=%u sites=%u alternatives=%u\n",
            ir->owner_pu_st, (UINT32)ir->descriptors.size(),
            (UINT32)ir->axes.size(), (UINT32)ir->blocks.size(),
            (UINT32)ir->sites.size(), (UINT32)ir->alternatives.size());
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_SITE_RECORD &site = ir->sites[i];
        fprintf(file, "  layout-site id=%u value=%u alternatives=%u selected=%u\n",
                site.id, site.semantic_value_id, site.alternative_count,
                site.selected_plan_id);
    }
}

/* Read the recorded PU symbol identity without consulting or activating a
 * PU; return ST_IDX_ZERO for NULL and leave the runtime store unchanged. */
ST_IDX DSL_logical_layout_ir_owner(const DSL_LOGICAL_LAYOUT_IR *ir)
{ return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st; }
/* Generate a read-only copied-table count accessor; a null runtime store
 * yields zero without consulting WHIRL or transferring ownership. */
#define DSL_LAYOUT_COUNT(name, field)                                    \
UINT32 DSL_logical_layout_ir_##name##_count(const DSL_LOGICAL_LAYOUT_IR *ir) \
{ return ir == NULL ? 0 : ir->field.size(); }
/* Read the copied descriptor table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
DSL_LAYOUT_COUNT(descriptor, descriptors)
/* Read the copied axis table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
DSL_LAYOUT_COUNT(axis, axes)
/* Read the copied block table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
DSL_LAYOUT_COUNT(block, blocks)
/* Read the copied site table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
DSL_LAYOUT_COUNT(site, sites)
/* Read the copied alternative table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
DSL_LAYOUT_COUNT(alternative, alternatives)
#undef DSL_LAYOUT_COUNT

/* Generate a copy-out lookup by one-based runtime table ID. Reject null
 * store/output, ID zero or ID beyond the selected vector, preserving any
 * supplied output on FALSE; success copies row id - 1 without mutating storage. */
#define DSL_LAYOUT_GET(name, type, record_type, field)                   \
BOOL DSL_logical_layout_ir_get_##name                                    \
        (const DSL_LOGICAL_LAYOUT_IR *ir, type id, record_type *record)   \
{                                                                        \
    if (ir == NULL || record == NULL || id == 0 || id > ir->field.size()) \
        return FALSE;                                                     \
    *record = ir->field[id - 1];                                         \
    return TRUE;                                                         \
}
/* Copy the descriptor row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
DSL_LAYOUT_GET(descriptor, DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID,
               DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD, descriptors)
/* Copy the axis row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
DSL_LAYOUT_GET(axis, DSL_LOGICAL_LAYOUT_AXIS_ID,
               DSL_LOGICAL_LAYOUT_AXIS_RECORD, axes)
/* Copy the block row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
DSL_LAYOUT_GET(block, DSL_LOGICAL_LAYOUT_BLOCK_ID,
               DSL_LOGICAL_LAYOUT_BLOCK_RECORD, blocks)
/* Copy the site row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
DSL_LAYOUT_GET(site, DSL_LOGICAL_LAYOUT_SITE_ID,
               DSL_LOGICAL_LAYOUT_SITE_RECORD, sites)
/* Copy the alternative row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
DSL_LAYOUT_GET(alternative, DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID,
               DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD, alternatives)
#undef DSL_LAYOUT_GET

/* Copy the first site with the requested nonzero semantic value ID into
 * caller-owned record. Return FALSE for a null store/output, zero value or no
 * match, preserving any supplied output and leaving the store unchanged. */
BOOL
DSL_logical_layout_ir_find_site
        (const DSL_LOGICAL_LAYOUT_IR *ir, DSL_IR_VALUE_ID value,
         DSL_LOGICAL_LAYOUT_SITE_RECORD *record)
{
    if (ir == NULL || record == NULL || value == 0)
        return FALSE;
    for (UINT32 i = 0; i < ir->sites.size(); ++i)
        if (ir->sites[i].semantic_value_id == value) {
            *record = ir->sites[i];
            return TRUE;
        }
    return FALSE;
}
