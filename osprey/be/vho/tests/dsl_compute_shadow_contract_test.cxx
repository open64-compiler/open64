/* Focused PU-local MMA2 model tests; synthetic timing is never a target
 * default. No compiler initialization or executable mutation.
 * Design: doc/AI-COMPILER-MMA2-SHADOW-MODEL.md. */
#include "../dsl_compute_shadow_opt.h"
#include "../../../common/com/tests/dsl_compute_test_fixture.h"
#include <limits.h>

/* Produce a fully explicit fictional scenario, not a hardware measurement. */
static DSL_COMPUTE_MODEL_RECORD VHO_DSL_test_model(UINT32 profile_id, UINT32 family)
{
    DSL_COMPUTE_MODEL_RECORD model;
    assert(DSL_compute_model_unknown(profile_id, family, &model));
    model.provenance = DSL_COMPUTE_MODEL_SYNTHETIC;
    model.unit = DSL_OPT_COST_UNIT_CYCLES; model.scenario_id = 123;
    model.configured = 1; model.sms = 2; model.resident_cap = 4; model.queue_capacity = 2;
    model.tmem_cap_known = 1; model.tmem_ctas_per_sm = 2;
    DSL_HARDWARE_RESOURCE_RECORD resource = {32, 64, 2048, 1024, 65536, 255, 256, 4, 233472, 65536, 128, 1024};
    model.resources = resource;
    const UINT64 nominal[] = {64, 128, 8192, 128, 64, 128, 64, 100, 4, 400, 8, 12, 16, 8, 20, 40};
    for (UINT32 index = 0; index < DSL_COMPUTE_MODEL_METRIC_COUNT; ++index) {
        model.metrics[index].known = 1;
        model.metrics[index].lower = nominal[index] / 2;
        model.metrics[index].nominal = nominal[index];
        model.metrics[index].upper = nominal[index] * 2;
    }
    assert(DSL_compute_model_verify(&model));
    return model;
}

/* Extend an exact MMA1 tuple across a rectangular grid and K sequence. */
static void VHO_DSL_test_geometry(DSL_COMPUTE_TEST_FIXTURE *fixture, UINT32 profile_id,
    UINT32 ctas, UINT32 atoms, UINT32 block_atoms)
{
    fixture->Initialize(profile_id);
    fixture->tile.problem_m *= ctas;
    fixture->tile.problem_k = atoms * 16;
    fixture->tile.cta_k = block_atoms * 16;
    fixture->tile.a_shared_stride = fixture->tile.cta_k;
    fixture->numerical.maximum_span = fixture->tile.problem_k;
    fixture->resources.storage.shared_bytes =
        (fixture->tile.cta_m + fixture->tile.cta_n) * fixture->tile.cta_k * 2;
    assert(DSL_compute_realization_verify(&fixture->record, &fixture->links, stderr));
}

/* Check complete attribution and independent arithmetic identities; preserve
 * a replayable stream for the separate Python dependency-graph oracle. */
static VHO_DSL_COMPUTE_SHADOW_RESULT VHO_DSL_test_evaluate(DSL_COMPUTE_TEST_FIXTURE &fixture,
    const DSL_COMPUTE_MODEL_RECORD &model, bool print)
{
    VHO_DSL_COMPUTE_SHADOW_RESULT result;
    assert(VHO_DSL_compute_shadow_analyze(&fixture.record, &fixture.links, &model, 1000000, &result, stderr));
    assert(result.known && result.useful_flops == result.executed_flops);
    for (UINT32 corner = 0; corner < 3; ++corner) {
        const VHO_DSL_COMPUTE_SHADOW_COST &cost = result.costs[corner];
        UINT64 sum = 0;
        for (UINT32 term = 0; term < VHO_DSL_SHADOW_TERM_COUNT; ++term) sum += cost.terms[term];
        assert(sum == cost.invocation && cost.floor <= cost.kernel);
        assert(cost.invocation >= cost.kernel);
    }
    if (print) assert(VHO_DSL_compute_shadow_print(stdout, &fixture.record, &fixture.links, &model, 1000000));
    return result;
}

/* Invalid model/contracts/budgets must leave the entire output and inspection
 * stream untouched, including partially derived work/resource facts. */
static void VHO_DSL_test_reject(DSL_COMPUTE_TEST_FIXTURE &fixture,
    const DSL_COMPUTE_MODEL_RECORD &model, UINT64 budget)
{
    VHO_DSL_COMPUTE_SHADOW_RESULT result, saved;
    memset(&result, 0x5a, sizeof(result)); saved = result;
    assert(!VHO_DSL_compute_shadow_analyze(&fixture.record, &fixture.links, &model, budget, &result, NULL));
    assert(memcmp(&result, &saved, sizeof(result)) == 0);
    FILE *file = tmpfile(); assert(file);
    assert(!VHO_DSL_compute_shadow_print(file, &fixture.record, &fixture.links, &model, budget));
    assert(ftell(file) == 0); fclose(file);
}

/* Cover all four exact recipes, both hypothetical families, queues, bounds,
 * occupancy and fail-closed boundaries without claiming native execution. */
int main()
{
    UINT32 cases = 0;
    for (UINT32 profile_id = 1; profile_id <= 4; ++profile_id) {
        DSL_COMPUTE_PROFILE_RECORD profile;
        assert(DSL_compute_profile_get(profile_id, &profile));
        assert(VHO_DSL_compute_shadow_eligibility(profile.arch_id, profile.input_format, profile_id) == VHO_DSL_SHADOW_COMPATIBLE);
        assert(VHO_DSL_compute_shadow_eligibility(profile.arch_id, DSL_ACC_FORMAT_F32, profile_id) == VHO_DSL_SHADOW_NUMERICAL);
        assert(VHO_DSL_compute_shadow_eligibility(0, profile.input_format, profile_id) == VHO_DSL_SHADOW_TARGET);
        for (UINT32 family_index = 0; family_index < 2; ++family_index) {
            UINT32 family = family_index ? profile.family : (UINT32)DSL_COMPUTE_SIMT;
            DSL_COMPUTE_MODEL_RECORD model = VHO_DSL_test_model(profile_id, family);
            const UINT32 grids[] = {1, 3, 8, 9};
            const UINT32 lengths[] = {1, 2, 6};
            for (UINT32 grid = 0; grid < 4; ++grid)
                for (UINT32 length = 0; length < 3; ++length)
                    for (UINT32 block_atoms = 1; block_atoms <= 2; ++block_atoms) {
                        if (lengths[length] % block_atoms) continue;
                        DSL_COMPUTE_TEST_FIXTURE fixture;
                        VHO_DSL_test_geometry(&fixture, profile_id, grids[grid], lengths[length], block_atoms);
                        for (UINT32 queue = 1; queue <= 4; queue *= 2) {
                            model.queue_capacity = queue;
                            VHO_DSL_test_evaluate(fixture, model, true); ++cases;
                        }
                    }
            DSL_COMPUTE_TEST_FIXTURE fixture;
            VHO_DSL_test_geometry(&fixture, profile_id, 9, 6, 2);
            model.queue_capacity = 1;
            VHO_DSL_COMPUTE_SHADOW_RESULT serial = VHO_DSL_test_evaluate(fixture, model, false);
            model.queue_capacity = 4;
            VHO_DSL_COMPUTE_SHADOW_RESULT parallel = VHO_DSL_test_evaluate(fixture, model, false);
            assert(serial.costs[1].queue_wait > parallel.costs[1].queue_wait);
            assert(serial.costs[1].kernel >= parallel.costs[1].kernel);
            for (UINT32 metric = 0; metric < DSL_COMPUTE_MODEL_METRIC_COUNT; ++metric) {
                DSL_COMPUTE_MODEL_RECORD missing = model;
                memset(&missing.metrics[metric], 0, sizeof(missing.metrics[metric]));
                VHO_DSL_COMPUTE_SHADOW_RESULT unknown;
                assert(VHO_DSL_compute_shadow_analyze(&fixture.record, &fixture.links, &missing, 1000000, &unknown, stderr));
                bool used = metric != DSL_COMPUTE_MODEL_TENSOR_FLOPS || family != DSL_COMPUTE_SIMT;
                if (metric == DSL_COMPUTE_MODEL_SCALAR_FLOPS || metric == DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS) used = family == DSL_COMPUTE_SIMT;
                if (metric == DSL_COMPUTE_MODEL_TMEM_BYTES || metric == DSL_COMPUTE_MODEL_ALLOC || metric == DSL_COMPUTE_MODEL_FREE) used = family == DSL_COMPUTE_TCGEN05;
                assert(unknown.known == !used);
            }
            DSL_COMPUTE_MODEL_RECORD unknown;
            assert(DSL_compute_model_unknown(profile_id, family, &unknown));
            VHO_DSL_COMPUTE_SHADOW_RESULT result;
            assert(VHO_DSL_compute_shadow_analyze(&fixture.record, &fixture.links, &unknown, 0, &result, stderr));
            assert(!result.known && !result.resident && !result.costs[1].invocation);
            assert(VHO_DSL_compute_shadow_print(stdout, &fixture.record, &fixture.links, &unknown, 0));
            if (family == DSL_COMPUTE_TCGEN05) {
                unknown = model; unknown.tmem_cap_known = unknown.tmem_ctas_per_sm = 0;
                assert(VHO_DSL_compute_shadow_analyze(&fixture.record, &fixture.links, &unknown, 0, &result, stderr));
                assert(!result.known);
            }
            VHO_DSL_test_reject(fixture, model, 1);
            DSL_COMPUTE_MODEL_RECORD invalid = model;
            invalid.metrics[0].lower = 0; VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model; invalid.metrics[8].upper = ~(UINT64)0; VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model; invalid.metrics[8].lower = invalid.metrics[8].upper + 1; VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model; invalid.provenance = DSL_COMPUTE_MODEL_UNKNOWN; VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model; invalid.sms = 1025; VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model; invalid.queue_capacity = 9; VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model; invalid.metrics[15].lower = invalid.metrics[15].nominal = invalid.metrics[15].upper = ~(UINT64)0 - 1;
            VHO_DSL_test_reject(fixture, invalid, 1000000);
            invalid = model;
            for (UINT32 metric = 7; metric < DSL_COMPUTE_MODEL_METRIC_COUNT; ++metric)
                invalid.metrics[metric].lower = invalid.metrics[metric].nominal = invalid.metrics[metric].upper = 0;
            VHO_DSL_test_evaluate(fixture, invalid, true); ++cases;
            DSL_COMPUTE_TEST_FIXTURE wide_grid;
            VHO_DSL_test_geometry(&wide_grid, profile_id, 129, 6, 1);
            invalid = model; invalid.sms = 132;
            VHO_DSL_test_evaluate(wide_grid, invalid, true); ++cases;
            fixture.tile.problem_m += 1; VHO_DSL_test_reject(fixture, model, 1000000);
            fixture.tile.problem_m = (~(UINT64)0 / fixture.tile.cta_m) * fixture.tile.cta_m;
            VHO_DSL_test_reject(fixture, model, 1000000);
        }
        DSL_COMPUTE_TEST_FIXTURE fixture;
        VHO_DSL_test_geometry(&fixture, profile_id, 9, 6, 1);
        DSL_COMPUTE_MODEL_RECORD scalar = VHO_DSL_test_model(profile_id, DSL_COMPUTE_SIMT);
        DSL_COMPUTE_MODEL_RECORD mma_model = VHO_DSL_test_model(profile_id, profile.family);
        VHO_DSL_COMPUTE_SHADOW_RESULT simt = VHO_DSL_test_evaluate(fixture, scalar, false);
        VHO_DSL_COMPUTE_SHADOW_RESULT mma = VHO_DSL_test_evaluate(fixture, mma_model, false);
        VHO_DSL_COMPUTE_SHADOW_COMPARISON comparison;
        assert(VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison));
        assert(comparison.nominal_family == profile.family && comparison.emitted_family == DSL_COMPUTE_SIMT);
        printf("# Conditional comparison profile=%u nominal=%u bounded=%u emitted=%u; synthetic only\n",
            profile_id, comparison.nominal_family, comparison.bounded_family, comparison.emitted_family);
        VHO_DSL_COMPUTE_SHADOW_RESULT saved = mma;
        ++mma.numerical.evidence_id;
        assert(!VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison)); mma = saved;
        ++mma.problem_m;
        assert(!VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison)); mma = saved;
        ++mma.costs[1].invocation;
        assert(!VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison)); mma = saved;
        ++mma.numerical_contract_id;
        assert(!VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison));
        mma = saved; mma.known = 0;
        assert(VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison));
        assert(!comparison.nominal_family && !comparison.bounded_family && comparison.emitted_family == DSL_COMPUTE_SIMT);
        for (UINT32 metric = 0; metric < DSL_COMPUTE_MODEL_METRIC_COUNT; ++metric) {
            scalar.metrics[metric].lower = scalar.metrics[metric].upper = scalar.metrics[metric].nominal;
            mma_model.metrics[metric].lower = mma_model.metrics[metric].upper = mma_model.metrics[metric].nominal;
        }
        simt = VHO_DSL_test_evaluate(fixture, scalar, false);
        mma = VHO_DSL_test_evaluate(fixture, mma_model, false);
        assert(VHO_DSL_compute_shadow_compare(&simt, &mma, &comparison));
        assert(comparison.bounded_family == profile.family && comparison.emitted_family == DSL_COMPUTE_SIMT);
    }
    printf("MMA2 shadow passed: %u complete synthetic schedules; real timing unknown; emitted SIMT unchanged\n", cases);
    return 0;
}
