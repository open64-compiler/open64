/* Borrowed, nonlowerable MMA test inputs. No target timing defaults.
 * Design: doc/AI-COMPILER-MMA2-SHADOW-MODEL.md. */
#ifndef dsl_compute_test_fixture_INCLUDED
#define dsl_compute_test_fixture_INCLUDED
#include "dsl_compute_realization.h"
#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>
#include <string.h>

struct DSL_COMPUTE_TEST_FIXTURE {
    DSL_COMPUTE_REALIZATION_RECORD record;
    DSL_COMPUTE_REALIZATION_LINKS links;
    DSL_TILE_SITE_RECORD site;
    DSL_TILE_PLAN_RECORD tile;
    DSL_OPT_PLAN_RECORD plan;
    DSL_TENSOR_EVOLUTION_NODE_RECORD nodes[4];
    DSL_NUMERICAL_CONTRACT_RECORD numerical;
    DSL_COMPUTE_PROTOCOL_RECORD protocol;
    DSL_COMPUTE_RESOURCE_RECORD resources;
    DSL_COMPUTE_FALLBACK_RECORD fallback;

    /* Build a borrowed inventory example, never a lowerable/native claim. */
    void Initialize(UINT32 profile_id)
    {
        memset(this, 0, sizeof(*this));
        DSL_COMPUTE_PROFILE_RECORD profile;
        assert(DSL_compute_profile_get(profile_id, &profile));
        record.id = 1; record.version = 1; record.owner_pu_st = 10;
        record.source_identity_id = 1; record.region_id = 2; record.semantic_node_id = 3;
        record.plan_id = 4; record.tile_plan_id = 5; record.profile_id = profile_id;
        record.descriptor_recipe = profile.descriptor_recipe;
        record.a_source_tier = record.b_source_tier = DSL_MEMORY_TIER_SHARED;
        record.fragment_mapping = profile.fragment_mapping;
        record.operand_nodes[0] = 11; record.operand_nodes[1] = 12;
        record.accumulator_node = 13; record.result_node = 14;
        record.numerical_contract_id = 6; record.numerical_acceptance_id = 7;
        record.protocol_id = 8; record.fallback_requirement_id = 9; record.generation = 1;
        record.required_ptx = profile.minimum_ptx; record.required_toolkit = profile.minimum_toolkit;
        record.lowering_support = record.native_support = DSL_COMPUTE_REQUIRED; record.source_line = 27;
        site.id = 1; site.owner_pu_st = 10; site.semantic_node_id = 3; site.semantic_value_id = 3;
        tile.id = 5; tile.site_id = 1;
        tile.target_profile_id = profile.family == DSL_COMPUTE_WGMMA ? DSL_TARGET_PROFILE_NVIDIA_HOPPER : DSL_TARGET_PROFILE_NVIDIA_BLACKWELL;
        tile.problem_m = tile.cta_m = tile.instruction_m = profile.instruction_m;
        tile.problem_n = tile.cta_n = tile.instruction_n = profile.instruction_n;
        tile.problem_k = tile.cta_k = tile.instruction_k = profile.instruction_k;
        tile.threads_per_cta = 128; tile.warps_per_cta = 4; tile.edge_policy = DSL_TILE_EDGE_EXACT;
        tile.a_shared_stride = 16;
        plan.id = 4; plan.owner_pu_st = 10; plan.flags = DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
        plan.legality = DSL_OPT_LEGALITY_UNKNOWN; plan.rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
        for (UINT32 index = 0; index < 4; ++index) {
            nodes[index].id = 11 + index; nodes[index].kind = DSL_TENSOR_EVOLUTION_NODE_INSTRUCTION_FRAGMENT;
            nodes[index].owner_pu_st = 10; nodes[index].semantic_value_id = index < 2 ? index + 1 : 3;
            nodes[index].descriptor_ty = 20 + index; nodes[index].semantic_root_id = index < 2 ? index + 1 : 3;
            nodes[index].flags = DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL;
            nodes[index].representation_descriptor_id = 30 + index;
        }
        numerical.id = 6; numerical.source_format = numerical.input_format = profile.input_format;
        numerical.product_format = numerical.partial_format = numerical.outer_format = numerical.output_format = DSL_ACC_FORMAT_F32;
        numerical.permissions = DSL_ACC_PERMISSION_REASSOCIATE; numerical.maximum_span = 16;
        numerical.evidence_kind = DSL_ACC_EVIDENCE_ISA; numerical.evidence_id = profile.evidence_toolkit;
        protocol.id = 8; protocol.recipe = profile.completion_recipe; protocol.obligations = profile.required_obligations;
        protocol.operand_ready = 1; protocol.issue = 2; protocol.completion = 3;
        protocol.result_read = 4; protocol.reuse_join = 5; protocol.outstanding_groups = 1;
        if (profile.family == DSL_COMPUTE_TCGEN05) {
            protocol.allocation = 6; protocol.deallocation = 7; protocol.permit_release = 8;
        }
        resources.storage.model = 1; resources.storage.register_bytes_per_thread = 128;
        resources.storage.register_capacity_per_thread = 1020; resources.storage.threads_per_cta = 128;
        resources.storage.thread_limit = 1024;
        resources.storage.shared_bytes = (tile.cta_m + tile.cta_n) * 16 * 2;
        resources.storage.shared_limit_bytes = 48 * 1024; resources.allocation_scope = DSL_MEMORY_SCOPE_CTA;
        resources.tmem_lanes = profile.tmem_lanes; resources.tmem_columns = resources.tmem_allocation_columns = profile.tmem_columns;
        fallback.id = 9; fallback.family = DSL_COMPUTE_SIMT; fallback.numerical_contract_id = 6;
        fallback.source_format = profile.input_format; fallback.output_format = DSL_ACC_FORMAT_F32;
        fallback.support = DSL_COMPUTE_REQUIRED; fallback.generation = 1;
        links.owner_pu_st = 10; links.arch_id = profile.arch_id;
        links.region_id = 2; links.source_identity_id = 1; links.generation = 1;
        links.numerical_acceptance_id = 7; links.site = &site; links.tile = &tile; links.plan = &plan;
        links.representations = nodes; links.representation_count = 4; links.numerical = &numerical;
        links.protocol = &protocol; links.resources = &resources; links.fallback = &fallback;
        assert(DSL_compute_realization_verify(&record, &links, stderr));
    }

    /* Verify atomic rejection and an empty inspection stream on bad input. */
    void Reject()
    {
        DSL_COMPUTE_REALIZATION_RECORD saved = record;
        FILE *file = tmpfile();
        assert(file != NULL);
        assert(!DSL_compute_realization_verify(&record, &links, NULL));
        assert(!DSL_compute_realization_print(file, &record, &links));
        assert(ftell(file) == 0 && memcmp(&record, &saved, sizeof(saved)) == 0);
        fclose(file);
    }
};
#endif
