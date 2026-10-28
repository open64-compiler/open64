/* VHO PU-local MMA5-H complete candidate cost/occupancy refinement of MMA2.
 * Caller timing is synthetic only; no target defaults or executable selection.
 * Design: doc/AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md. */
#include "dsl_wgmma_pipeline_opt.h"
#include "dsl_gemm_occupancy.h"
#include <string.h>

/* MMA2's range convention: service rates reverse the duration corner. */
static uint64_t VHO_DSL_pipeline_metric(const DSL_COMPUTE_MODEL_RECORD &model, UINT32 metric, UINT32 corner)
{
    const DSL_COMPUTE_MODEL_RANGE &range = model.metrics[metric];
    if (corner == 1) return range.nominal;
    return (corner == 0) == (metric > DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS) ? range.lower : range.upper;
}

/* Add exactly one modeled service, rejecting arithmetic overflow atomically. */
static BOOL VHO_DSL_pipeline_charge(uint64_t *value, uint64_t count, uint64_t duration)
{
    uint64_t amount;
    return DSL_readiness_mul(count, duration, &amount) && DSL_readiness_add(*value, amount, value);
}

/* Serialize shared engines within each SM and global traffic across the
 * device. No memory/compute overlap, latency hiding or persistent RF is
 * assumed. Residency rounds include the actual last CTA/column underfill. */
static BOOL VHO_DSL_pipeline_cost(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_COMPUTE_MODEL_RECORD &model, UINT32 candidate_id, UINT32 corner,
    VHO_DSL_WGMMA_PIPELINE_CANDIDATE *candidate)
{
    const BOOL simt = candidate_id == 0, tma = candidate_id == 3;
    const uint64_t steps = record.problem_k / 16, slots = candidate->slots;
    const uint64_t blocks_x = simt ? DSL_readiness_ceil(record.problem_n, 128) : record.problem_n / 64;
    const uint64_t ctas = blocks_x * (simt ? record.problem_m : record.problem_m / 64);
    uint64_t capacity;
    if (!DSL_readiness_mul(model.sms, candidate->resident, &capacity)) return FALSE;
    uint64_t total[8] = {};
    for (uint64_t first = 0; first < ctas; first += capacity) {
        const uint64_t active = std::min(capacity, ctas - first);
        std::vector<uint64_t> sm_terms(model.sms * 7, 0);
        uint64_t device_input = 0, device_output = 0;
        for (uint64_t offset = 0; offset < active; ++offset) {
            uint64_t *terms = &sm_terms[(offset % model.sms) * 7];
            uint64_t columns = simt ? std::min(UINT64_C(128), (uint64_t)record.problem_n - ((first + offset) % blocks_x) * 128) : 64;
            uint64_t outputs = simt ? columns : 4096;
            uint64_t batches = simt ? record.problem_k : DSL_readiness_ceil(steps, slots);
            uint64_t flops, input, shared, temporary;
            if (!DSL_readiness_mul(outputs, (uint64_t)record.problem_k * 2, &flops) ||
                !DSL_readiness_mul(simt ? outputs * 4 : 4096, simt ? record.problem_k : steps, &input) ||
                !DSL_readiness_mul(simt ? 0 : (tma ? 16384 : 8192), steps, &shared) ||
                !DSL_readiness_mul(simt ? 0 : 32768, batches, &temporary) ||
                !DSL_readiness_add(input, temporary, &input) ||
                !DSL_readiness_add(input, simt ? 0 : 32768, &input) ||
                !DSL_readiness_add(device_input, input, &device_input) ||
                !VHO_DSL_pipeline_charge(&device_output, outputs, 4)) return FALSE;
            if (!VHO_DSL_pipeline_charge(&terms[1], 1, DSL_readiness_ceil(shared,
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_SHARED_BYTES, corner))) ||
                !VHO_DSL_pipeline_charge(&terms[2], 1, DSL_readiness_ceil(flops,
                    VHO_DSL_pipeline_metric(model, simt ? DSL_COMPUTE_MODEL_SCALAR_FLOPS : DSL_COMPUTE_MODEL_TENSOR_FLOPS, corner))) ||
                !VHO_DSL_pipeline_charge(&terms[3], simt ? 0 : steps + (tma ? steps * 80 : 0),
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_ISSUE, corner)) ||
                !VHO_DSL_pipeline_charge(&terms[4], simt ? 0 : batches * (tma ? 3 : 2),
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_SYNC, corner)) ||
                !VHO_DSL_pipeline_charge(&terms[5], simt ? 0 : steps,
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_RESULT_LATENCY, corner)) ||
                !VHO_DSL_pipeline_charge(&terms[5], simt ? record.problem_k : (tma ? batches : steps),
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_GLOBAL_LATENCY, corner))) return FALSE;
            if (!VHO_DSL_pipeline_charge(&terms[6], 1, DSL_readiness_ceil(simt ? flops : 0,
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS, corner))) ||
                !VHO_DSL_pipeline_charge(&terms[6], 1, DSL_readiness_ceil(outputs,
                    VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_EPILOGUE_ELEMENTS, corner))) ||
                !VHO_DSL_pipeline_charge(&terms[6], 1, VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_PROLOGUE, corner)) ||
                !VHO_DSL_pipeline_charge(&terms[6], simt ? 0 : 1, VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_ALLOC, corner)) ||
                !VHO_DSL_pipeline_charge(&terms[6], simt ? 0 : 1, VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_FREE, corner)) ||
                !VHO_DSL_pipeline_charge(&terms[6], simt ? 0 : steps, VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_PREPARATION, corner))) return FALSE;
        }
        uint64_t longest = 0; UINT32 winner = 0;
        for (UINT32 sm = 0; sm < model.sms; ++sm) {
            uint64_t duration = 0;
            for (UINT32 term = 1; term < 7; ++term)
                if (!DSL_readiness_add(duration, sm_terms[sm * 7 + term], &duration)) return FALSE;
            if (duration > longest) { longest = duration; winner = sm; }
        }
        for (UINT32 term = 1; term < 7; ++term)
            if (!DSL_readiness_add(total[term], sm_terms[winner * 7 + term], &total[term])) return FALSE;
        if (!VHO_DSL_pipeline_charge(&total[0], 1, DSL_readiness_ceil(device_input,
                VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_DEVICE_BYTES, corner))) ||
            !VHO_DSL_pipeline_charge(&total[0], 1, DSL_readiness_ceil(device_output,
                VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_DEVICE_BYTES, corner)))) return FALSE;
    }
    uint64_t kernel = 0;
    for (UINT32 term = 0; term < 7; ++term)
        if (!DSL_readiness_add(kernel, total[term], &kernel)) return FALSE;
    total[7] = VHO_DSL_pipeline_metric(model, DSL_COMPUTE_MODEL_LAUNCH, corner);
    if (!DSL_readiness_add(kernel, total[7], &candidate->invocation[corner])) return FALSE;
    candidate->kernel[corner] = kernel;
    memcpy(candidate->terms[corner], total, sizeof(total));
    return TRUE;
}

/* Enumerate complete SIMT and implemented Hopper choices under one source
 * contract. Unknown timing keeps all costs unavailable and emitted SIMT;
 * only a strict non-overlapping synthetic envelope names a bounded winner. */
BOOL VHO_DSL_wgmma_pipeline_analyze(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_PIPELINE_CONTROL *control,
    const DSL_COMPUTE_MODEL_RECORD *model, uint64_t register_bytes,
    VHO_DSL_WGMMA_PIPELINE_RESULT *result, FILE *diagnostic)
{
    DSL_COMPUTE_PROFILE_RECORD profile;
    if (!result || !control || !DSL_typed_simt_verify(record, numerical, diagnostic) ||
        !DSL_compute_model_verify(model) || !DSL_compute_profile_get(control->compute.profile_id, &profile) ||
        profile.family != DSL_COMPUTE_WGMMA || model->compute_profile_id != profile.id ||
        model->family != DSL_COMPUTE_WGMMA || control->reserved ||
        (control->delivery != VHO_DSL_WGMMA_COOPERATIVE && control->delivery != VHO_DSL_WGMMA_TMA_BULK) ||
        !register_bytes || register_bytes < 256 ||
        record->problem_k > 1048576 || record->threads_per_cta != 128) return FALSE;
    VHO_DSL_WGMMA_PIPELINE_RESULT output = {};
    output.emitted_family = DSL_COMPUTE_SIMT;
    const BOOL compatible = control->compute.enable == 1 && control->compute.reserved == 0 &&
        control->compute.acceptance_flags == VHO_DSL_WGMMA_ACCEPT_ISA &&
        control->compute.arch_id == profile.arch_id && control->compute.ptx >= profile.minimum_ptx &&
        control->compute.toolkit >= profile.minimum_toolkit && profile.input_format == numerical->input_format &&
        numerical->permissions == DSL_ACC_PERMISSION_REASSOCIATE && record->problem_m % 64 == 0 &&
        record->problem_n % 64 == 0 && record->problem_k % 16 == 0 && control->outstanding_groups == 2;
    BOOL timing = model->configured != 0;
    for (UINT32 metric = 0; metric < DSL_COMPUTE_MODEL_METRIC_COUNT; ++metric)
        timing = timing && model->metrics[metric].known;
    if (model->configured && (model->sms > 1024 || model->resident_cap > 32 || model->queue_capacity > 8)) return FALSE;
    for (UINT32 candidate_id = 0; candidate_id < 4; ++candidate_id) {
        VHO_DSL_WGMMA_PIPELINE_CANDIDATE &candidate = output.candidates[candidate_id];
        candidate.family = candidate_id ? DSL_COMPUTE_WGMMA : DSL_COMPUTE_SIMT;
        candidate.delivery = candidate_id == 3 ? 2 : 1; candidate.slots = candidate_id < 2 ? 1 : 2;
        candidate.groups = candidate_id ? candidate.slots : 0;
        candidate.shared_bytes = candidate_id == 3 ? 16400 : candidate_id ? 4096 * candidate.slots : 0;
        candidate.eligible = !candidate_id || (compatible && control->compute.shared_limit_bytes >= candidate.shared_bytes &&
            (candidate_id != 3 || (control->compute.ptx >= 806 && control->compute.toolkit >= 12080)));
        if (!candidate.eligible || !model->configured) continue;
        const uint64_t ctas = candidate_id ? (uint64_t)(record->problem_m / 64) * (record->problem_n / 64) :
            (uint64_t)record->problem_m * DSL_readiness_ceil(record->problem_n, 128);
        if (ctas > 65536 || ctas * DSL_readiness_ceil(record->problem_k, 16) > 1048576) return FALSE;
        DSL_GEMM_OCCUPANCY_RESULT occupancy;
        if (!DSL_gemm_occupancy(model->resources, 32, model->sms, 128, register_bytes, candidate.shared_bytes, ctas, &occupancy)) return FALSE;
        candidate.rounded_registers = occupancy.registers; candidate.rounded_shared = occupancy.shared;
        candidate.resident = std::min(occupancy.resident, (uint64_t)model->resident_cap);
        candidate.limiting = occupancy.limiting | (candidate.resident == model->resident_cap ? 32 : 0);
        if (!candidate.resident || model->queue_capacity < candidate.groups) { candidate.eligible = 0; continue; }
        if (!timing) continue;
        for (UINT32 corner = 0; corner < 3; ++corner)
            if (!VHO_DSL_pipeline_cost(*record, *model, candidate_id, corner, &candidate)) return FALSE;
        if (candidate.invocation[0] > candidate.invocation[1] || candidate.invocation[1] > candidate.invocation[2]) return FALSE;
        candidate.known = 1;
    }
    UINT32 nominal = 0;
    for (UINT32 candidate_id = 1; candidate_id < 4; ++candidate_id)
        if (output.candidates[candidate_id].known && (!output.candidates[nominal].known ||
            output.candidates[candidate_id].invocation[1] < output.candidates[nominal].invocation[1])) nominal = candidate_id;
    output.nominal_candidate = nominal;
    if (output.candidates[nominal].known) {
        BOOL separated = TRUE;
        for (UINT32 candidate_id = 0; candidate_id < 4; ++candidate_id)
            if (candidate_id != nominal && output.candidates[candidate_id].eligible &&
                (!output.candidates[candidate_id].known || output.candidates[nominal].invocation[2] >= output.candidates[candidate_id].invocation[0])) separated = FALSE;
        if (separated) output.bounded_candidate = nominal;
    }
    *result = output;
    return TRUE;
}
