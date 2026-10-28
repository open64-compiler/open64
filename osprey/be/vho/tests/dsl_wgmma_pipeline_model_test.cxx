/* PU-local MMA5-H model stress without WHIRL construction or target execution.
 * Covers multiple residency rounds, queue/resource rejection and unknowns.
 * Design: doc/AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md. */
#include "dsl_wgmma_pipeline_opt.h"
#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>
#include <string.h>

/* Exercise complete tuples with independently checked resource invariants and
 * exact ledger reconciliation. No synthetic result promotes execution. */
int main()
{
    UINT32 scenarios = 0, costs = 0;
    const UINT32 shapes[][3] = {{64, 64, 16}, {512, 512, 80}, {64, 192, 48}, {1024, 64, 128}};
    for (UINT32 profile = 1; profile <= 2; ++profile)
        for (UINT32 shape = 0; shape < 4; ++shape)
            for (UINT32 sms = 1; sms <= 3; ++sms)
                for (UINT32 queue = 1; queue <= 3; ++queue)
                    for (UINT32 budget = 8192; budget <= 32768; budget *= 2) {
                        DSL_TYPED_SIMT_RECORD record = {};
                        record.id = record.version = record.owner_pu_st = record.generation = record.numerical_contract_id = record.numerical_acceptance_id = record.disjoint_output = 1;
                        record.problem_m = shapes[shape][0]; record.problem_n = shapes[shape][1]; record.problem_k = shapes[shape][2]; record.threads_per_cta = 128;
                        DSL_NUMERICAL_CONTRACT_RECORD numerical = {};
                        numerical.id = 1; numerical.source_format = numerical.input_format = profile + 4;
                        numerical.product_format = numerical.partial_format = numerical.outer_format = numerical.output_format = DSL_ACC_FORMAT_F32;
                        numerical.permissions = DSL_ACC_PERMISSION_REASSOCIATE; numerical.maximum_span = record.problem_k;
                        numerical.evidence_kind = DSL_ACC_EVIDENCE_ISA; numerical.evidence_id = 12081;
                        VHO_DSL_WGMMA_PIPELINE_CONTROL control = {};
                        control.compute.enable = 1; control.compute.profile_id = profile; control.compute.arch_id = DSL_HARDWARE_ARCH_SM_90A;
                        control.compute.ptx = 807; control.compute.toolkit = 12081; control.compute.acceptance_flags = 31;
                        control.compute.shared_limit_bytes = 16400; control.delivery = 2; control.outstanding_groups = 2;
                        DSL_COMPUTE_MODEL_RECORD model;
                        assert(DSL_compute_model_unknown(profile, DSL_COMPUTE_WGMMA, &model));
                        VHO_DSL_WGMMA_PIPELINE_RESULT result;
                        assert(VHO_DSL_wgmma_pipeline_analyze(&record, &numerical, &control, &model, 384, &result, stderr));
                        assert(result.emitted_family == DSL_COMPUTE_SIMT && !result.bounded_candidate);
                        for (UINT32 candidate_id = 0; candidate_id < 4; ++candidate_id) assert(!result.candidates[candidate_id].known);
                        model.provenance = DSL_COMPUTE_MODEL_SYNTHETIC; model.unit = DSL_OPT_COST_UNIT_CYCLES; model.scenario_id = 100 + scenarios;
                        model.configured = 1; model.sms = sms; model.resident_cap = 4; model.queue_capacity = queue;
                        DSL_HARDWARE_RESOURCE_RECORD resources = {32, 64, 2048, 1024, 65536, 255, 256, 4, 233472, 65536, 128, 1024};
                        resources.shared_budget = budget; model.resources = resources;
                        const uint64_t nominal[] = {64, 128, 8192, 128, 64, 128, 64, 100, 4, 400, 8, 12, 16, 8, 20, 40};
                        for (UINT32 metric = 0; metric < 16; ++metric) {
                            model.metrics[metric].known = 1; model.metrics[metric].lower = nominal[metric] / 2;
                            model.metrics[metric].nominal = nominal[metric]; model.metrics[metric].upper = nominal[metric] * 2;
                        }
                        assert(VHO_DSL_wgmma_pipeline_analyze(&record, &numerical, &control, &model, 384, &result, stderr));
                        assert(result.emitted_family == DSL_COMPUTE_SIMT);
                        for (UINT32 candidate_id = 0; candidate_id < 4; ++candidate_id) {
                            const VHO_DSL_WGMMA_PIPELINE_CANDIDATE &candidate = result.candidates[candidate_id];
                            assert(candidate.known == candidate.eligible);
                            if (!candidate.known) continue;
                            assert(candidate.resident <= 4 && candidate.groups <= queue);
                            assert(candidate.resident * candidate.rounded_shared <= budget);
                            assert(candidate.resident * candidate.rounded_registers <= 65536);
                            for (UINT32 corner = 0; corner < 3; ++corner) {
                                uint64_t sum = 0;
                                for (UINT32 term = 0; term < 8; ++term) sum += candidate.terms[corner][term];
                                assert(sum == candidate.invocation[corner]);
                                assert(candidate.kernel[corner] + candidate.terms[corner][7] == sum);
                                if (corner) assert(candidate.invocation[corner - 1] <= sum);
                                ++costs;
                            }
                        }
                        ++scenarios;
                    }
    printf("MMA5-H model stress passed: scenarios=%u unknown_scenarios=%u cost_corners=%u residency_rounds=many default=SIMT timing=unknown\n", scenarios, scenarios, costs);
    return 0;
}
