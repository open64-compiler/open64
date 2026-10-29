/* Standalone common/targ_info/access hardware-description contract fixture.
 * Exercise immutable rows, exact keys, failure-output preservation and the
 * limited structural predicates without backend headers or PU initialization.
 * No device execution or performance claim. Design:
 * doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md. */
#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "dsl_memory_hierarchy.h"
#include "dsl_compute_profile.h"

/* Print explicit fields, not object padding or process-specific string pointers,
 * so the same fixture can compare pre-move and relocated inventory bytes. */
static void DSL_test_dump_words(const UINT64 *values, UINT32 count)
{
    for (UINT32 index = 0; index < count; ++index)
        printf(" %llu", (unsigned long long)values[index]);
    printf("\n");
}

/* Retain every quantitative field of a copied architecture row. Borrow strings
 * and leave the row unchanged; this is inventory evidence, not calibration. */
static void DSL_test_dump_description(const DSL_HARDWARE_DESCRIPTION_RECORD &row)
{
    const DSL_HARDWARE_PERFORMANCE_RECORD &performance = row.performance;
    const DSL_HARDWARE_RESOURCE_RECORD &resources = row.resources;
    const UINT64 values[] = {
        row.arch_id, row.target_profile_id, row.revision, row.latency_override,
        performance.sms, performance.warp_lanes, performance.memory_bytes_per_cycle,
        performance.scalar_ops_per_cycle, performance.local_issues_per_cycle,
        performance.copy_elements_per_cycle, performance.latency_hiding_warps,
        performance.demand_latency, performance.staged_latency, performance.tma_latency,
        performance.store_latency, performance.publication_cycles, performance.wait_cycles,
        performance.release_cycles, performance.operand_issue_cycles,
        resources.max_blocks, resources.max_warps, resources.max_threads,
        resources.max_threads_per_cta, resources.registers_per_sm,
        resources.max_registers_per_thread, resources.register_quantum,
        resources.register_warp_quantum, resources.shared_capacity,
        resources.shared_budget, resources.shared_quantum, resources.shared_reserved
    };
    printf("architecture %s %s %s %s", row.architecture, row.table_name,
           row.unit, row.provenance);
    DSL_test_dump_words(values, sizeof(values) / sizeof(values[0]));
}

/* Retain every profile field with stable numeric formatting. Copying an
 * inventory support label must not turn it into emission or timing evidence. */
static void DSL_test_dump_profile(const DSL_COMPUTE_PROFILE_RECORD &row)
{
    const UINT64 values[] = {
        row.id, row.revision, row.arch_id, row.family, row.input_format,
        row.accumulator_format, row.instruction_m, row.instruction_n,
        row.instruction_k, row.cta_group, row.threads_per_cta, row.mma_issuers,
        row.allocation_threads, row.fragment_mapping, row.descriptor_recipe,
        row.accumulator_tier, row.accumulator_registers_per_thread,
        row.tmem_lanes, row.tmem_columns, row.tmem_min_allocation,
        row.tmem_max_allocation, row.descriptor_alignment, row.minimum_ptx,
        row.minimum_toolkit, row.evidence_ptx, row.evidence_toolkit,
        row.completion_recipe, row.required_obligations, row.lowering_support,
        row.native_support
    };
    printf("compute %s %s", row.architecture, row.variant);
    DSL_test_dump_words(values, sizeof(values) / sizeof(values[0]));
}

/* Exercise rejected/null/missing lookups using sentinel outputs. Compare bytes
 * only when no assignment is allowed, never across successful padded copies. */
static void DSL_test_failures()
{
    DSL_HARDWARE_DESCRIPTION_RECORD description;
    memset(&description, 0xa5, sizeof(description));
    unsigned char saved_description[sizeof(description)];
    memcpy(saved_description, &description, sizeof(description));
    assert(!DSL_hardware_description_get_arch(0, &description));
    assert(!DSL_hardware_description_get_arch(999, &description));
    assert(!DSL_hardware_description_get_arch(1, NULL));
    const char *unknown_names[] = {"", "sm_9", "sm_90A", "SM_90a", "sm_90a-extra", "sm_120"};
    for (UINT32 index = 0; index < sizeof(unknown_names) / sizeof(unknown_names[0]); ++index)
        assert(!DSL_hardware_description_find_arch(unknown_names[index], &description));
    assert(!DSL_hardware_description_find_arch(NULL, &description));
    assert(!DSL_hardware_description_find_arch("sm_90a", NULL));
    assert(memcmp(&description, &saved_description, sizeof(description)) == 0);

    DSL_COMPUTE_PROFILE_RECORD profile;
    memset(&profile, 0xa5, sizeof(profile));
    unsigned char saved_profile[sizeof(profile)];
    memcpy(saved_profile, &profile, sizeof(profile));
    assert(!DSL_compute_profile_get(0, &profile));
    assert(!DSL_compute_profile_get(5, &profile));
    assert(!DSL_compute_profile_get(1, NULL));
    assert(!DSL_compute_profile_find(NULL, "variant", &profile));
    assert(!DSL_compute_profile_find("sm_90a", NULL, &profile));
    assert(!DSL_compute_profile_find("sm_90", "wgmma_m64n64k16_f16_ss", &profile));
    assert(!DSL_compute_profile_find("sm_90a", "wgmma_m64n64k16_F16_ss", &profile));
    assert(!DSL_compute_profile_find("sm_90a", "wgmma_m64n64k16_f16_ss", NULL));
    assert(memcmp(&profile, &saved_profile, sizeof(profile)) == 0);

    DSL_MEMORY_TIER_RECORD tier;
    memset(&tier, 0xa5, sizeof(tier));
    unsigned char saved_tier[sizeof(tier)];
    memcpy(saved_tier, &tier, sizeof(tier));
    assert(!DSL_memory_hierarchy_get_tier(0, 1, &tier));
    assert(!DSL_memory_hierarchy_get_tier(2, 0, &tier));
    assert(!DSL_memory_hierarchy_get_tier(2, 7, &tier));
    assert(!DSL_memory_hierarchy_find_tier(2, DSL_MEMORY_TIER_REMOTE, &tier));
    assert(!DSL_memory_hierarchy_get_tier(2, 1, NULL));
    assert(memcmp(&tier, &saved_tier, sizeof(tier)) == 0);

    DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD movement;
    memset(&movement, 0xa5, sizeof(movement));
    unsigned char saved_movement[sizeof(movement)];
    memcpy(saved_movement, &movement, sizeof(movement));
    assert(!DSL_memory_hierarchy_get_movement(0, 1, &movement));
    assert(!DSL_memory_hierarchy_get_movement(2, 0, &movement));
    assert(!DSL_memory_hierarchy_get_movement(2, 5, &movement));
    assert(!DSL_memory_hierarchy_find_movement(2, 999, &movement));
    assert(!DSL_memory_hierarchy_get_movement(2, 1, NULL));
    assert(memcmp(&movement, &saved_movement, sizeof(movement)) == 0);

    DSL_SHARED_BANK_MODEL_RECORD bank = {7, 8, 9};
    assert(!DSL_memory_hierarchy_shared_bank_model(1, &bank));
    assert(!DSL_memory_hierarchy_shared_bank_model(0, &bank));
    assert(!DSL_memory_hierarchy_shared_bank_model(2, NULL));
    assert(bank.bank_count == 7 && bank.word_bytes == 8 && bank.warp_lanes == 9);
    DSL_MEMORY_HIERARCHY_PROFILE family;
    memset(&family, 0xa5, sizeof(family));
    unsigned char saved_family[sizeof(family)];
    memcpy(saved_family, &family, sizeof(family));
    assert(!DSL_memory_hierarchy_get_profile(0, &family));
    assert(!DSL_memory_hierarchy_get_profile(1, NULL));
    assert(memcmp(&family, &saved_family, sizeof(family)) == 0);
    assert(DSL_memory_hierarchy_movement_count(0) == 0);
    assert(!DSL_memory_hierarchy_validate(0, NULL));
    FILE *diagnostic = tmpfile();
    assert(diagnostic != NULL);
    DSL_memory_hierarchy_print(diagnostic, 0);
    assert(ftell(diagnostic) == 0);
    assert(!DSL_memory_hierarchy_validate(0, diagnostic));
    assert(ftell(diagnostic) > 0);
    fclose(diagnostic);
}

/* Compare all built-in rows and predicate boundaries without initializing a PU,
 * linking a backend, selecting a plan or changing any WHIRL table. */
int main()
{
    DSL_test_failures();
    assert(DSL_ACC_FORMAT_F32 == 1 && DSL_ACC_FORMAT_F16 == 5 && DSL_ACC_FORMAT_BF16 == 6);
    for (UINT32 family = 1; family <= 3; ++family) {
        assert(DSL_memory_hierarchy_validate(family, stderr));
        DSL_memory_hierarchy_print(stdout, family);
        if (family > 1) {
            DSL_SHARED_BANK_MODEL_RECORD bank;
            assert(DSL_memory_hierarchy_shared_bank_model(family, &bank));
            assert(bank.bank_count == 32 && bank.word_bytes == 4 && bank.warp_lanes == 32);
        }
    }
    for (UINT32 arch_id = 1; arch_id <= 4; ++arch_id) {
        DSL_HARDWARE_DESCRIPTION_RECORD description;
        DSL_HARDWARE_DESCRIPTION_RECORD found;
        assert(DSL_hardware_description_get_arch(arch_id, &description));
        assert(DSL_hardware_description_find_arch(description.architecture, &found));
        assert(found.arch_id == arch_id && found.revision == description.revision);
        DSL_test_dump_description(description);
        DSL_HARDWARE_PERFORMANCE_RECORD performance = description.performance;
        performance.demand_latency = performance.wait_cycles = 0;
        assert(DSL_hardware_performance_valid(performance));
        performance.memory_bytes_per_cycle = 0;
        assert(!DSL_hardware_performance_valid(performance));
        DSL_HARDWARE_RESOURCE_RECORD resources = description.resources;
        resources.shared_reserved = resources.shared_capacity + 1;
        assert(DSL_hardware_resources_valid(resources));
        resources.shared_budget = resources.shared_capacity + 1;
        assert(!DSL_hardware_resources_valid(resources));
        resources = description.resources;
        resources.register_quantum = 0;
        assert(!DSL_hardware_resources_valid(resources));
    }
    for (UINT32 profile_id = 1; profile_id <= 4; ++profile_id) {
        DSL_COMPUTE_PROFILE_RECORD profile;
        DSL_COMPUTE_PROFILE_RECORD found;
        assert(DSL_compute_profile_get(profile_id, &profile));
        assert(DSL_compute_profile_find(profile.architecture, profile.variant, &found));
        assert(found.id == profile_id && profile.accumulator_format == DSL_ACC_FORMAT_F32);
        assert(profile.lowering_support == DSL_COMPUTE_REQUIRED && profile.native_support == DSL_COMPUTE_REQUIRED);
        DSL_test_dump_profile(profile);
    }
    assert(strcmp(DSL_target_profile_name(999), "unknown") == 0);
    assert(strcmp(DSL_memory_tier_name(999), "unknown") == 0);
    assert(strcmp(DSL_memory_scope_name(999), "unknown") == 0);
    assert(strcmp(DSL_memory_movement_name(999), "unknown") == 0);
    printf("hardware contract passed: families=3 architectures=4 compute_profiles=4 exact_keys=true failure_outputs=unchanged timing=unchanged\n");
    return 0;
}
