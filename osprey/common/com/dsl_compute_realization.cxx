/* Policy-free MMA1 inventory and structural joins in caller-owned PU scope.
 * No WN creation, mapped-image changes, rate model, legality policy or CUDA
 * emission. Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
#include "dsl_compute_realization.h"
#include <string.h>

/* This intentionally bounded inventory is narrower than the ISA. Evidence is
 * CUDA 12.8.1/PTX 8.7; minimum_toolkit is our conservative feature floor. */
static const DSL_COMPUTE_PROFILE_RECORD DSL_compute_profiles[] = {
    {1, 1, DSL_HARDWARE_ARCH_SM_90A, "sm_90a", "wgmma_m64n64k16_f16_ss", DSL_COMPUTE_WGMMA,
     DSL_ACC_FORMAT_F16, DSL_ACC_FORMAT_F32, 64, 64, 16, 1, 128, 128, 0,
     DSL_COMPUTE_FRAGMENT_WGMMA_64_64, 1, DSL_COMPUTE_ACCUMULATOR_RF, 32, 0, 0, 0, 0,
     16, 800, 12000, 807, 12081, 1, 127, DSL_COMPUTE_REQUIRED, DSL_COMPUTE_REQUIRED},
    {2, 1, DSL_HARDWARE_ARCH_SM_90A, "sm_90a", "wgmma_m64n64k16_bf16_ss", DSL_COMPUTE_WGMMA,
     DSL_ACC_FORMAT_BF16, DSL_ACC_FORMAT_F32, 64, 64, 16, 1, 128, 128, 0,
     DSL_COMPUTE_FRAGMENT_WGMMA_64_64, 1, DSL_COMPUTE_ACCUMULATOR_RF, 32, 0, 0, 0, 0,
     16, 800, 12000, 807, 12081, 1, 127, DSL_COMPUTE_REQUIRED, DSL_COMPUTE_REQUIRED},
    {3, 1, DSL_HARDWARE_ARCH_SM_100A, "sm_100a", "tcgen05_m128n64k16_f16_ss", DSL_COMPUTE_TCGEN05,
     DSL_ACC_FORMAT_F16, DSL_ACC_FORMAT_F32, 128, 64, 16, 1, 128, 1, 32,
     DSL_COMPUTE_FRAGMENT_TCGEN05_128_64, 2, DSL_COMPUTE_ACCUMULATOR_TMEM, 0, 128, 64, 32, 512,
     16, 806, 12080, 807, 12081, 2, 4091, DSL_COMPUTE_REQUIRED, DSL_COMPUTE_REQUIRED},
    {4, 1, DSL_HARDWARE_ARCH_SM_100A, "sm_100a", "tcgen05_m128n64k16_bf16_ss", DSL_COMPUTE_TCGEN05,
     DSL_ACC_FORMAT_BF16, DSL_ACC_FORMAT_F32, 128, 64, 16, 1, 128, 1, 32,
     DSL_COMPUTE_FRAGMENT_TCGEN05_128_64, 2, DSL_COMPUTE_ACCUMULATOR_TMEM, 0, 128, 64, 32, 512,
     16, 806, 12080, 807, 12081, 2, 4091, DSL_COMPUTE_REQUIRED, DSL_COMPUTE_REQUIRED}
};

/* Immutable table lookup publishes only a complete known row. */
BOOL DSL_compute_profile_get(DSL_COMPUTE_PROFILE_ID id,
    DSL_COMPUTE_PROFILE_RECORD *profile)
{
    if (profile == NULL || id == 0 || id > sizeof(DSL_compute_profiles) / sizeof(DSL_compute_profiles[0]))
        return FALSE;
    *profile = DSL_compute_profiles[id - 1];
    return TRUE;
}

/* Names are exact keys, never architecture-family capability inference. */
BOOL DSL_compute_profile_find(const char *architecture, const char *variant,
    DSL_COMPUTE_PROFILE_RECORD *profile)
{
    if (architecture == NULL || variant == NULL || profile == NULL)
        return FALSE;
    for (UINT32 index = 0; index < sizeof(DSL_compute_profiles) / sizeof(DSL_compute_profiles[0]); ++index) {
        const DSL_COMPUTE_PROFILE_RECORD &entry = DSL_compute_profiles[index];
        if (strcmp(architecture, entry.architecture) == 0 && strcmp(variant, entry.variant) == 0) {
            *profile = entry;
            return TRUE;
        }
    }
    return FALSE;
}

/* A diagnostic is optional; no plan or image is ever published on failure. */
static BOOL DSL_compute_error(FILE *diagnostic, const char *reason)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "compute realization: %s\n", reason);
    return FALSE;
}

/* Validate only structural references and the reviewed bounded inventory.
 * Alias, exact descriptor/fragment bit mapping, progress and numerical proofs
 * remain prerequisites for future VHO legality and WHIRL materialization. */
BOOL DSL_compute_realization_verify(const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, FILE *diagnostic)
{
    if (record == NULL || links == NULL || links->site == NULL || links->tile == NULL ||
        links->plan == NULL || links->representations == NULL || links->representation_count != 4 ||
        links->numerical == NULL || links->protocol == NULL || links->resources == NULL || links->fallback == NULL)
        return DSL_compute_error(diagnostic, "missing links");
    DSL_COMPUTE_PROFILE_RECORD profile;
    if (!DSL_compute_profile_get(record->profile_id, &profile) || profile.arch_id != links->arch_id)
        return DSL_compute_error(diagnostic, "unknown profile");
    const DSL_TILE_SITE_RECORD &site = *links->site;
    const DSL_TILE_PLAN_RECORD &tile = *links->tile;
    const DSL_OPT_PLAN_RECORD &plan = *links->plan;
    if (!record->id || record->version != 1 || record->reserved || !record->source_line ||
        !record->owner_pu_st || record->owner_pu_st != links->owner_pu_st ||
        site.owner_pu_st != record->owner_pu_st || plan.owner_pu_st != record->owner_pu_st ||
        !record->region_id || record->region_id != links->region_id ||
        !record->source_identity_id || record->source_identity_id != links->source_identity_id ||
        !record->semantic_node_id || record->semantic_node_id != site.semantic_node_id ||
        !record->generation || record->generation != links->generation ||
        !record->plan_id || record->plan_id != plan.id ||
        !record->tile_plan_id || record->tile_plan_id != tile.id ||
        !site.id || tile.site_id != site.id || site.reserved || tile.reserved)
        return DSL_compute_error(diagnostic, "identity or invalidation mismatch");
    if (record->lowering_support != DSL_COMPUTE_REQUIRED || record->native_support != DSL_COMPUTE_REQUIRED ||
        plan.flags != DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY || plan.cost_id || plan.fallback_plan_id ||
        plan.legality != DSL_OPT_LEGALITY_UNKNOWN || plan.rejection_reason != DSL_OPT_REJECT_INCOMPLETE_ANALYSIS ||
        record->required_ptx != profile.minimum_ptx || record->required_toolkit != profile.minimum_toolkit)
        return DSL_compute_error(diagnostic, "planning-only or toolchain boundary");
    UINT32 target = profile.family == DSL_COMPUTE_WGMMA ? DSL_TARGET_PROFILE_NVIDIA_HOPPER : DSL_TARGET_PROFILE_NVIDIA_BLACKWELL;
    if (record->descriptor_recipe != profile.descriptor_recipe ||
        record->a_source_tier != DSL_MEMORY_TIER_SHARED || record->b_source_tier != DSL_MEMORY_TIER_SHARED ||
        record->fragment_mapping != profile.fragment_mapping ||
        tile.target_profile_id != target || tile.instruction_family != DSL_TILE_INSTRUCTION_UNKNOWN ||
        tile.instruction_m != profile.instruction_m || tile.instruction_n != profile.instruction_n ||
        tile.instruction_k != profile.instruction_k || tile.cta_m != profile.instruction_m ||
        tile.cta_n != profile.instruction_n || !tile.cta_k || tile.cta_k % profile.instruction_k ||
        !tile.problem_m || !tile.problem_n || !tile.problem_k || tile.problem_m % tile.cta_m ||
        tile.problem_n % tile.cta_n || tile.problem_k % tile.cta_k ||
        tile.threads_per_cta != profile.threads_per_cta || tile.warps_per_cta != 4 ||
        tile.edge_policy != DSL_TILE_EDGE_EXACT || tile.a_shared_layout != DSL_TILE_A_LAYOUT_ROW_MAJOR ||
        tile.b_shared_layout != DSL_TILE_B_LAYOUT_ROW_MAJOR || tile.a_shared_stride != tile.cta_k)
        return DSL_compute_error(diagnostic, "unsupported geometry or representation recipe");
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical = *links->numerical;
    if (!record->numerical_contract_id || record->numerical_contract_id != numerical.id || numerical.reserved ||
        numerical.source_format != profile.input_format || numerical.input_format != profile.input_format ||
        numerical.product_format != DSL_ACC_FORMAT_F32 || numerical.partial_format != DSL_ACC_FORMAT_F32 ||
        numerical.outer_format != DSL_ACC_FORMAT_F32 || numerical.output_format != DSL_ACC_FORMAT_F32 ||
        numerical.permissions != DSL_ACC_PERMISSION_REASSOCIATE || numerical.maximum_span < tile.problem_k ||
        numerical.evidence_kind != DSL_ACC_EVIDENCE_ISA || numerical.evidence_id != profile.evidence_toolkit ||
        !record->numerical_acceptance_id || record->numerical_acceptance_id != links->numerical_acceptance_id)
        return DSL_compute_error(diagnostic, "numerical contract mismatch");
    const DSL_TENSOR_EVOLUTION_NODE_ID expected[] = {record->operand_nodes[0], record->operand_nodes[1],
        record->accumulator_node, record->result_node};
    for (UINT32 index = 0; index < 4; ++index) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &node = links->representations[index];
        if (!expected[index] || node.id != expected[index] || node.owner_pu_st != record->owner_pu_st ||
            !node.semantic_value_id || !node.descriptor_ty || !node.semantic_root_id || !node.representation_descriptor_id ||
            node.kind != DSL_TENSOR_EVOLUTION_NODE_INSTRUCTION_FRAGMENT ||
            node.flags != DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL || node.reserved)
            return DSL_compute_error(diagnostic, "unresolved representation identity");
        for (UINT32 previous = 0; previous < index; ++previous)
            if (node.id == links->representations[previous].id)
                return DSL_compute_error(diagnostic, "duplicate representation identity");
        if (index >= 2 && node.semantic_value_id != site.semantic_value_id)
            return DSL_compute_error(diagnostic, "result semantic value mismatch");
    }
    const DSL_COMPUTE_FALLBACK_RECORD &fallback = *links->fallback;
    if (!record->fallback_requirement_id || record->fallback_requirement_id != fallback.id ||
        fallback.family != DSL_COMPUTE_SIMT || fallback.numerical_contract_id != numerical.id ||
        fallback.source_format != numerical.source_format || fallback.output_format != numerical.output_format ||
        fallback.support != DSL_COMPUTE_REQUIRED || fallback.plan_id || fallback.reserved ||
        fallback.generation != record->generation)
        return DSL_compute_error(diagnostic, "same-contract fallback requirement mismatch");
    const DSL_COMPUTE_PROTOCOL_RECORD &protocol = *links->protocol;
    if (!record->protocol_id || record->protocol_id != protocol.id || protocol.recipe != profile.completion_recipe ||
        protocol.obligations != profile.required_obligations || protocol.outstanding_groups != 1 || protocol.reserved)
        return DSL_compute_error(diagnostic, "completion recipe mismatch");
    const UINT32 events[] = {protocol.operand_ready, protocol.issue, protocol.completion,
        protocol.result_read, protocol.reuse_join, protocol.allocation, protocol.deallocation, protocol.permit_release};
    for (UINT32 index = 0; index < 8; ++index) {
        if ((index < 5 || profile.family == DSL_COMPUTE_TCGEN05) != (events[index] != 0))
            return DSL_compute_error(diagnostic, "missing or extraneous event requirement");
        for (UINT32 previous = 0; previous < index; ++previous)
            if (events[index] && events[index] == events[previous])
                return DSL_compute_error(diagnostic, "issue is not completion or reusable storage");
    }
    const DSL_COMPUTE_RESOURCE_RECORD &resources = *links->resources;
    const DSL_FETCH_RESOURCE_RECORD &storage = resources.storage;
    UINT64 minimum_shared = (UINT64)(tile.cta_m + tile.cta_n) * tile.cta_k * 2;
    if (resources.reserved || storage.reserved || storage.model != 1 ||
        storage.threads_per_cta != profile.threads_per_cta || !storage.thread_limit ||
        storage.threads_per_cta > storage.thread_limit || !storage.register_capacity_per_thread ||
        storage.register_bytes_per_thread > storage.register_capacity_per_thread ||
        storage.register_bytes_per_thread < profile.accumulator_registers_per_thread * 4 ||
        storage.shared_bytes < minimum_shared || storage.shared_bytes > storage.shared_limit_bytes ||
        !storage.shared_limit_bytes || resources.allocation_scope != DSL_MEMORY_SCOPE_CTA)
        return DSL_compute_error(diagnostic, "invalid resource requirements");
    if (resources.tmem_lanes != profile.tmem_lanes || resources.tmem_columns != profile.tmem_columns ||
        (profile.family == DSL_COMPUTE_WGMMA ? resources.tmem_allocation_columns != 0 :
         (resources.tmem_allocation_columns < profile.tmem_columns ||
          resources.tmem_allocation_columns < profile.tmem_min_allocation ||
          resources.tmem_allocation_columns > profile.tmem_max_allocation ||
          (resources.tmem_allocation_columns & (resources.tmem_allocation_columns - 1)) != 0)))
        return DSL_compute_error(diagnostic, "invalid TMEM allocation requirement");
    return TRUE;
}

/* Emit an isolated analysis contract, including unknown timing and absent
 * lowerability; source line is the final field on every candidate row. */
BOOL DSL_compute_realization_print(FILE *file, const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links)
{
    if (file == NULL || !DSL_compute_realization_verify(record, links, NULL))
        return FALSE;
    DSL_COMPUTE_PROFILE_RECORD profile;
    DSL_compute_profile_get(record->profile_id, &profile);
    fprintf(file, "# What it summarizes: MMA1 structural compute inventory and required fallback/resources/events; no selection or WHIRL actions.\n");
    fprintf(file, "# AI_COMPUTE 1 PROFILE id revision arch variant family M N K threads issuers alloc_threads fragment accumulator_storage acc_rf tmem_lanes tmem_cols ptx_floor toolkit_floor evidence_ptx evidence_toolkit rate latency lowering native\n");
    fprintf(file, "AI_COMPUTE 1 PROFILE %u %u %s %s %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u unknown unknown required required\n",
        profile.id, profile.revision, profile.architecture, profile.variant, profile.family,
        profile.instruction_m, profile.instruction_n, profile.instruction_k, profile.threads_per_cta,
        profile.mma_issuers, profile.allocation_threads, profile.fragment_mapping, profile.accumulator_tier,
        profile.accumulator_registers_per_thread, profile.tmem_lanes, profile.tmem_columns,
        profile.minimum_ptx, profile.minimum_toolkit, profile.evidence_ptx, profile.evidence_toolkit);
    fprintf(file, "# AI_COMPUTE 1 LIMITS profile cta_group descriptor_alignment tmem_min tmem_max descriptor_recipe completion_recipe capability descriptor_state\n");
    fprintf(file, "AI_COMPUTE 1 LIMITS %u %u %u %u %u %u %u isa_documented encoding_required\n",
        profile.id, profile.cta_group, profile.descriptor_alignment, profile.tmem_min_allocation,
        profile.tmem_max_allocation, profile.descriptor_recipe, profile.completion_recipe);
    fprintf(file, "# AI_COMPUTE 1 CAND id owner source region node plan tile numerical acceptance protocol fallback generation status source_line\n");
    fprintf(file, "AI_COMPUTE 1 CAND %u %u %u %u %u %u %u %u %u %u %u %llu planning_only %u\n",
        record->id, record->owner_pu_st, record->source_identity_id, record->region_id,
        record->semantic_node_id, record->plan_id, record->tile_plan_id, record->numerical_contract_id,
        record->numerical_acceptance_id, record->protocol_id, record->fallback_requirement_id,
        (unsigned long long)record->generation, record->source_line);
    fprintf(file, "# AI_COMPUTE 1 REPRESENTATION candidate role node value tensor_ty semantic_root representation descriptor_recipe source_tier source_line\n");
    for (UINT32 index = 0; index < 4; ++index) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &node = links->representations[index];
        UINT32 tier = index < 2 ? DSL_MEMORY_TIER_SHARED : 0;
        fprintf(file, "AI_COMPUTE 1 REPRESENTATION %u %u %u %u %u %u %u %u %u %u\n",
            record->id, index, node.id, node.semantic_value_id, node.descriptor_ty,
            node.semantic_root_id, node.representation_descriptor_id, record->descriptor_recipe,
            tier, record->source_line);
    }
    fprintf(file, "# AI_COMPUTE 1 PROTOCOL id recipe required_bits ready issue complete read reuse alloc free permit outstanding proof source_line\n");
    const DSL_COMPUTE_PROTOCOL_RECORD &protocol = *links->protocol;
    fprintf(file, "AI_COMPUTE 1 PROTOCOL %u %u %u %u %u %u %u %u %u %u %u %u required %u\n",
        protocol.id, protocol.recipe, protocol.obligations, protocol.operand_ready, protocol.issue,
        protocol.completion, protocol.result_read, protocol.reuse_join, protocol.allocation,
        protocol.deallocation, protocol.permit_release, protocol.outstanding_groups, record->source_line);
    fprintf(file, "# AI_COMPUTE 1 RESOURCE id logical_rf_bytes rf_limit shared_bytes shared_limit threads thread_limit tmem_lanes tmem_columns alloc_columns scope native_registers source_line\n");
    const DSL_COMPUTE_RESOURCE_RECORD &resources = *links->resources;
    fprintf(file, "AI_COMPUTE 1 RESOURCE %u %u %u %llu %llu %u %u %u %u %u %u unknown %u\n", record->id,
        resources.storage.register_bytes_per_thread, resources.storage.register_capacity_per_thread,
        (unsigned long long)resources.storage.shared_bytes, (unsigned long long)resources.storage.shared_limit_bytes,
        resources.storage.threads_per_cta, resources.storage.thread_limit,
        resources.tmem_lanes, resources.tmem_columns, resources.tmem_allocation_columns,
        resources.allocation_scope, record->source_line);
    fprintf(file, "# AI_COMPUTE 1 FALLBACK requirement family contract source output plan status source_line\n");
    const DSL_COMPUTE_FALLBACK_RECORD &fallback = *links->fallback;
    fprintf(file, "AI_COMPUTE 1 FALLBACK %u simt %u %u %u %u required_unimplemented %u\n",
        fallback.id, fallback.numerical_contract_id, fallback.source_format, fallback.output_format,
        fallback.plan_id, record->source_line);
    return !ferror(file);
}
