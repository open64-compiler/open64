/* Focused MMA1 inventory tests with actual common records; no WHIRL mutation.
 * Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
#include "dsl_compute_realization.h"
#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "dsl_compute_test_fixture.h"

/* Test all four exact recipes and malformed linkage without initializing a
 * compiler, producing an image or silently substituting scalar F32 buffers. */
int main()
{
    unsigned negatives = 0;
    for (UINT32 ident = 1; ident <= 4; ++ident) {
        DSL_COMPUTE_TEST_FIXTURE fixture;
        fixture.Initialize(ident);
        DSL_COMPUTE_PROFILE_RECORD profile, found;
        assert(DSL_compute_profile_get(ident, &profile));
        assert(DSL_compute_profile_find(profile.architecture, profile.variant, &found));
        assert(found.id == ident && found.lowering_support == DSL_COMPUTE_REQUIRED);
        assert(profile.instruction_n == 64 && profile.instruction_k == 16 && profile.evidence_ptx == 807);
        assert(profile.mma_issuers == (ident <= 2 ? 128 : 1));
        assert(profile.accumulator_registers_per_thread == (ident <= 2 ? 32 : 0));
        const char *bad_architectures[] = {"sm_90", "sm_100", "sm_120", "sm_120a", "sm_100f", "sm_101a", "SM_90a", "sm_90a ", ""};
        for (UINT32 index = 0; index < sizeof(bad_architectures) / sizeof(bad_architectures[0]); ++index) {
            found = profile;
            assert(!DSL_compute_profile_find(bad_architectures[index], profile.variant, &found));
            assert(memcmp(&profile, &found, sizeof(profile)) == 0);
            ++negatives;
        }
        assert(!DSL_compute_profile_find(profile.architecture, "mma.sync", &found));
        assert(!DSL_compute_profile_find(NULL, profile.variant, &found));
        assert(!DSL_compute_profile_get(0, &found) && !DSL_compute_profile_get(5, &found));
        assert(!DSL_compute_profile_get(1, NULL));
        assert(memcmp(&profile, &found, sizeof(profile)) == 0);
        const size_t offsets[] = {
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, version), offsetof(DSL_COMPUTE_REALIZATION_RECORD, owner_pu_st),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, region_id), offsetof(DSL_COMPUTE_REALIZATION_RECORD, source_identity_id),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, semantic_node_id), offsetof(DSL_COMPUTE_REALIZATION_RECORD, plan_id),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, tile_plan_id), offsetof(DSL_COMPUTE_REALIZATION_RECORD, numerical_contract_id),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, numerical_acceptance_id), offsetof(DSL_COMPUTE_REALIZATION_RECORD, protocol_id),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, fallback_requirement_id), offsetof(DSL_COMPUTE_REALIZATION_RECORD, lowering_support),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, native_support), offsetof(DSL_COMPUTE_REALIZATION_RECORD, reserved),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, descriptor_recipe), offsetof(DSL_COMPUTE_REALIZATION_RECORD, a_source_tier),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, b_source_tier), offsetof(DSL_COMPUTE_REALIZATION_RECORD, fragment_mapping),
            offsetof(DSL_COMPUTE_REALIZATION_RECORD, required_ptx), offsetof(DSL_COMPUTE_REALIZATION_RECORD, required_toolkit)};
        for (UINT32 index = 0; index < sizeof(offsets) / sizeof(offsets[0]); ++index) {
            fixture.Initialize(ident);
            ++*reinterpret_cast<UINT32 *>(reinterpret_cast<char *>(&fixture.record) + offsets[index]);
            fixture.Reject(); ++negatives;
        }
        for (UINT32 fault = 0; fault < 40; ++fault) {
            fixture.Initialize(ident);
            switch (fault) {
            case 0: fixture.record.profile_id = 99; break;
            case 1: fixture.record.generation = 2; break;
            case 2: fixture.tile.cta_n = 48; break;
            case 3: fixture.tile.instruction_k = 32; break;
            case 4: fixture.tile.problem_m += 1; break;
            case 5: fixture.tile.a_shared_layout = DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED; break;
            case 6: fixture.tile.b_shared_layout = DSL_TILE_B_LAYOUT_XOR_COLUMNS; break;
            case 7: fixture.tile.threads_per_cta = 64; break;
            case 8: fixture.numerical.source_format = DSL_ACC_FORMAT_F32; break;
            case 9: fixture.numerical.input_format = DSL_ACC_FORMAT_F8_E4M3; break;
            case 10: fixture.numerical.output_format = DSL_ACC_FORMAT_F16; break;
            case 11: fixture.numerical.permissions = 0; break;
            case 12: fixture.numerical.permissions |= DSL_ACC_PERMISSION_NARROW; break;
            case 13: fixture.numerical.maximum_span = 8; break;
            case 14: fixture.numerical.evidence_id = 0; break;
            case 15: fixture.fallback.source_format = DSL_ACC_FORMAT_F32; break;
            case 16: fixture.fallback.numerical_contract_id = 99; break;
            case 17: fixture.fallback.support = DSL_COMPUTE_IMPLEMENTED; break;
            case 18: fixture.fallback.plan_id = 50; break;
            case 19: fixture.fallback.generation = 2; break;
            case 20: fixture.protocol.completion = fixture.protocol.issue; break;
            case 21: fixture.protocol.reuse_join = 0; break;
            case 22: fixture.protocol.obligations ^= DSL_COMPUTE_OBLIGATION_PROXY; break;
            case 23: fixture.protocol.outstanding_groups = 2; break;
            case 24: fixture.resources.tmem_allocation_columns = ident <= 2 ? 32 : 48; break;
            case 25: fixture.resources.tmem_lanes += 1; break;
            case 26: fixture.resources.storage.shared_bytes = 0; break;
            case 27: fixture.resources.storage.register_bytes_per_thread = 1024; break;
            case 28: fixture.resources.storage.reserved = 1; break;
            case 29: fixture.nodes[0].representation_descriptor_id = 0; break;
            case 30: fixture.nodes[1].id = fixture.nodes[0].id; break;
            case 31: fixture.nodes[2].semantic_value_id = 99; break;
            case 32: fixture.nodes[3].owner_pu_st = 99; break;
            case 33: fixture.links.tile = NULL; break;
            case 34: fixture.links.representation_count = 3; break;
            case 35: fixture.plan.flags = 0; break;
            case 36: fixture.tile.instruction_family = DSL_TILE_INSTRUCTION_SCALAR_FMA; break;
            case 37: fixture.tile.target_profile_id = DSL_TARGET_PROFILE_CPU_BASELINE; break;
            case 38: fixture.protocol.permit_release = ident <= 2 ? 9 : 0; break;
            case 39: fixture.links.arch_id = DSL_HARDWARE_ARCH_SM_90; break;
            }
            fixture.Reject(); ++negatives;
        }
        fixture.Initialize(ident);
        assert(DSL_compute_realization_print(stdout, &fixture.record, &fixture.links));
    }
    assert(!DSL_compute_realization_verify(NULL, NULL, NULL));
    printf("MMA1 contract passed: 4 exact profiles, %u malformed cases; no selection/lowering/native certification\n", negatives);
    return 0;
}
