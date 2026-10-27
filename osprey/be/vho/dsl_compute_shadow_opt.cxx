/* PU-local MMA2 analytical schedules and shadow comparisons. All timing and
 * configured resource data are explicit caller descriptions. No WHIRL or
 * executable selection mutation. Design: doc/AI-COMPILER-MMA2-SHADOW-MODEL.md. */
#include "dsl_compute_shadow_opt.h"
#include "dsl_gemm_occupancy.h"
#include <algorithm>
#include <vector>
#include <string.h>

enum { VHO_DSL_SHADOW_MAX_SMS = 1024, VHO_DSL_SHADOW_MAX_CTAS = 65536, VHO_DSL_SHADOW_MAX_QUEUE = 8 };

struct VHO_DSL_SHADOW_POINT {
    uint64_t time;
    uint64_t terms[VHO_DSL_SHADOW_TERM_COUNT];
};

/* An optional diagnostic never changes caller-owned records or output. */
static BOOL VHO_DSL_shadow_error(FILE *file, const char *reason)
{
    if (file) fprintf(file, "compute shadow: %s\n", reason);
    return FALSE;
}

/* Deterministic max-plus tie order defines the attribution convention. */
static VHO_DSL_SHADOW_POINT VHO_DSL_shadow_max(const VHO_DSL_SHADOW_POINT &first,
    const VHO_DSL_SHADOW_POINT &second)
{
    return second.time > first.time ? second : first;
}

/* Extend one critical path, preserving its exact additive attribution. */
static bool VHO_DSL_shadow_advance(VHO_DSL_SHADOW_POINT *point, uint64_t duration, UINT32 term)
{
    return DSL_readiness_add(point->time, duration, &point->time) &&
        DSL_readiness_add(point->terms[term], duration, &point->terms[term]);
}

/* Select a consistent rate/duration corner; fast uses upper service rates. */
static uint64_t VHO_DSL_shadow_metric(const DSL_COMPUTE_MODEL_RECORD &model, UINT32 metric, UINT32 corner)
{
    const DSL_COMPUTE_MODEL_RANGE &range = model.metrics[metric];
    if (corner == 1) return range.nominal;
    bool fast = corner == 0;
    if (metric <= DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS) fast = !fast;
    return fast ? range.lower : range.upper;
}

/* Exact target/type filtering cannot narrow an existing F32 program. */
UINT32 VHO_DSL_compute_shadow_eligibility(UINT32 arch_id, UINT32 format, DSL_COMPUTE_PROFILE_ID profile_id)
{
    DSL_COMPUTE_PROFILE_RECORD profile;
    if (!DSL_compute_profile_get(profile_id, &profile) || arch_id != profile.arch_id) return VHO_DSL_SHADOW_TARGET;
    return format == profile.input_format ? VHO_DSL_SHADOW_COMPATIBLE : VHO_DSL_SHADOW_NUMERICAL;
}

/* Execute one bounded round with explicit resource calendars. Every CTA owns
 * one staging buffer and waits for its preceding accumulator completion.
 * Calendar points carry dependency paths, not native instruction evidence. */
static BOOL VHO_DSL_shadow_round(const DSL_COMPUTE_PROFILE_RECORD &profile,
    const DSL_COMPUTE_MODEL_RECORD &model, const DSL_TILE_PLAN_RECORD &tile,
    uint64_t active, UINT32 corner, VHO_DSL_COMPUTE_SHADOW_COST *cost)
{
    const bool simt = model.family == DSL_COMPUTE_SIMT;
    const bool tmem = !simt && profile.family == DSL_COMPUTE_TCGEN05;
    const uint64_t input = (uint64_t)(tile.cta_m + tile.cta_n) * tile.cta_k * 2;
    const uint64_t operands = (uint64_t)(profile.instruction_m + profile.instruction_n) * profile.instruction_k * 2;
    const uint64_t output = (uint64_t)tile.cta_m * tile.cta_n * 4;
    const uint64_t flops = (uint64_t)2 * profile.instruction_m * profile.instruction_n * profile.instruction_k;
    const uint64_t steps = tile.problem_k / tile.cta_k, atoms_per_step = tile.cta_k / profile.instruction_k;
    std::vector<VHO_DSL_SHADOW_POINT> scalar(model.sms), tensor(model.sms), shared(model.sms), reads(model.sms);
    std::vector<VHO_DSL_SHADOW_POINT> queues(model.sms * model.queue_capacity);
    std::vector<uint64_t> issued(model.sms), compute_busy(model.sms), shared_busy(model.sms);
    std::vector<uint64_t> scalar_busy(model.sms), tmem_busy(model.sms);
    std::vector<VHO_DSL_SHADOW_POINT> actor(active), ready(active);
    VHO_DSL_SHADOW_POINT memory = {}, finished = {};
    uint64_t operand_wait = 0, accumulator_wait = 0, queue_wait = 0, resource_wait = 0, device_bytes = 0;
    const uint64_t device_rate = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_DEVICE_BYTES, corner);
    const uint64_t shared_rate = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_SHARED_BYTES, corner);
    const uint64_t issue = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_ISSUE, corner);
    const uint64_t sync = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_SYNC, corner);
    const uint64_t latency = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_RESULT_LATENCY, corner);
    const uint64_t global_latency = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_GLOBAL_LATENCY, corner);
    const uint64_t service = DSL_readiness_ceil(flops, VHO_DSL_shadow_metric(model,
        simt ? DSL_COMPUTE_MODEL_SCALAR_FLOPS : DSL_COMPUTE_MODEL_TENSOR_FLOPS, corner));
    for (uint64_t cta = 0; cta < active; ++cta) {
        UINT32 sm = cta % model.sms;
        actor[cta] = scalar[sm];
        uint64_t setup = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_PROLOGUE, corner);
        if (tmem && !DSL_readiness_add(setup, VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_ALLOC, corner), &setup)) return FALSE;
        if (!VHO_DSL_shadow_advance(&actor[cta], setup, VHO_DSL_SHADOW_ISSUE) ||
            !DSL_readiness_add(scalar_busy[sm], setup, &scalar_busy[sm])) return FALSE;
        scalar[sm] = actor[cta];
    }
    for (uint64_t step = 0; step < steps; ++step) {
        for (uint64_t cta = 0; cta < active; ++cta) {
            UINT32 sm = cta % model.sms;
            VHO_DSL_SHADOW_POINT transfer = VHO_DSL_shadow_max(actor[cta], memory);
            if (!VHO_DSL_shadow_advance(&transfer, DSL_readiness_ceil(input, device_rate), VHO_DSL_SHADOW_OPERAND) ||
                !DSL_readiness_add(device_bytes, input, &device_bytes)) return FALSE;
            memory = transfer;
            if (!VHO_DSL_shadow_advance(&transfer, global_latency, VHO_DSL_SHADOW_OPERAND)) return FALSE;
            ready[cta] = VHO_DSL_shadow_max(transfer, shared[sm]);
            uint64_t shared_service = DSL_readiness_ceil(input, shared_rate);
            if (!VHO_DSL_shadow_advance(&ready[cta], shared_service, VHO_DSL_SHADOW_OPERAND) ||
                !DSL_readiness_add(shared_busy[sm], shared_service, &shared_busy[sm])) return FALSE;
            shared[sm] = ready[cta];
            ready[cta] = VHO_DSL_shadow_max(ready[cta], scalar[sm]);
            uint64_t conversion = simt ? DSL_readiness_ceil(input / 2,
                VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS, corner)) : 0;
            if (!VHO_DSL_shadow_advance(&ready[cta], conversion, VHO_DSL_SHADOW_OPERAND) ||
                !VHO_DSL_shadow_advance(&ready[cta], sync, VHO_DSL_SHADOW_SYNC) ||
                !DSL_readiness_add(scalar_busy[sm], conversion, &scalar_busy[sm]) ||
                !DSL_readiness_add(scalar_busy[sm], sync, &scalar_busy[sm])) return FALSE;
            scalar[sm] = ready[cta];
        }
        for (uint64_t atom = 0; atom < atoms_per_step; ++atom) {
            for (uint64_t cta = 0; cta < active; ++cta) {
                UINT32 sm = cta % model.sms;
                VHO_DSL_SHADOW_POINT begin = ready[cta];
                uint64_t start = begin.time;
                begin = VHO_DSL_shadow_max(begin, actor[cta]);
                if (!DSL_readiness_add(accumulator_wait, begin.time - start, &accumulator_wait)) return FALSE;
                start = begin.time;
                UINT32 slot = issued[sm] % model.queue_capacity;
                begin = VHO_DSL_shadow_max(begin, queues[sm * model.queue_capacity + slot]);
                if (!DSL_readiness_add(queue_wait, begin.time - start, &queue_wait)) return FALSE;
                start = begin.time;
                begin = VHO_DSL_shadow_max(begin, shared[sm]);
                if (!DSL_readiness_add(resource_wait, begin.time - start, &resource_wait)) return FALSE;
                uint64_t shared_service = DSL_readiness_ceil(operands, shared_rate);
                if (!VHO_DSL_shadow_advance(&begin, shared_service, VHO_DSL_SHADOW_OPERAND) ||
                    !DSL_readiness_add(shared_busy[sm], shared_service, &shared_busy[sm]) ||
                    !DSL_readiness_add(operand_wait, shared_service, &operand_wait)) return FALSE;
                shared[sm] = begin;
                start = begin.time;
                begin = VHO_DSL_shadow_max(begin, scalar[sm]);
                if (!simt) begin = VHO_DSL_shadow_max(begin, tensor[sm]);
                if (!DSL_readiness_add(resource_wait, begin.time - start, &resource_wait)) return FALSE;
                uint64_t issue_start = begin.time;
                if (!VHO_DSL_shadow_advance(&begin, issue, VHO_DSL_SHADOW_ISSUE) ||
                    !DSL_readiness_add(scalar_busy[sm], issue, &scalar_busy[sm])) return FALSE;
                scalar[sm] = begin;
                if (!VHO_DSL_shadow_advance(&begin, service, VHO_DSL_SHADOW_COMPUTE) ||
                    !DSL_readiness_add(compute_busy[sm], service, &compute_busy[sm])) return FALSE;
                if (simt) {
                    scalar[sm] = begin;
                    if (!DSL_readiness_add(scalar_busy[sm], service, &scalar_busy[sm])) return FALSE;
                } else tensor[sm] = begin;
                uint64_t completion;
                if (!DSL_readiness_add(issue_start, latency, &completion)) return FALSE;
                if (completion > begin.time && !VHO_DSL_shadow_advance(&begin, completion - begin.time, VHO_DSL_SHADOW_COMPLETION)) return FALSE;
                actor[cta] = begin;
                queues[sm * model.queue_capacity + slot] = begin;
                ++issued[sm];
            }
        }
        for (uint64_t cta = 0; cta < active; ++cta) {
            UINT32 sm = cta % model.sms;
            actor[cta] = VHO_DSL_shadow_max(actor[cta], scalar[sm]);
            if (!VHO_DSL_shadow_advance(&actor[cta], sync, VHO_DSL_SHADOW_SYNC) ||
                !DSL_readiness_add(scalar_busy[sm], sync, &scalar_busy[sm])) return FALSE;
            scalar[sm] = actor[cta];
        }
    }
    for (uint64_t cta = 0; cta < active; ++cta) {
        UINT32 sm = cta % model.sms;
        VHO_DSL_SHADOW_POINT drain = actor[cta];
        if (tmem) {
            drain = VHO_DSL_shadow_max(drain, reads[sm]);
            uint64_t read_service = DSL_readiness_ceil(output, VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_TMEM_BYTES, corner));
            if (!VHO_DSL_shadow_advance(&drain, read_service, VHO_DSL_SHADOW_EPILOGUE) ||
                !DSL_readiness_add(tmem_busy[sm], read_service, &tmem_busy[sm])) return FALSE;
            reads[sm] = drain;
        }
        drain = VHO_DSL_shadow_max(drain, scalar[sm]);
        uint64_t epilogue = DSL_readiness_ceil(output / 4, VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_EPILOGUE_ELEMENTS, corner));
        if (!VHO_DSL_shadow_advance(&drain, epilogue, VHO_DSL_SHADOW_EPILOGUE) ||
            !DSL_readiness_add(scalar_busy[sm], epilogue, &scalar_busy[sm])) return FALSE;
        scalar[sm] = drain;
        drain = VHO_DSL_shadow_max(drain, memory);
        if (!VHO_DSL_shadow_advance(&drain, DSL_readiness_ceil(output, device_rate), VHO_DSL_SHADOW_OUTPUT) ||
            !DSL_readiness_add(device_bytes, output, &device_bytes)) return FALSE;
        memory = drain;
        if (!VHO_DSL_shadow_advance(&drain, global_latency, VHO_DSL_SHADOW_OUTPUT)) return FALSE;
        if (tmem) {
            drain = VHO_DSL_shadow_max(drain, scalar[sm]);
            uint64_t free_service = VHO_DSL_shadow_metric(model, DSL_COMPUTE_MODEL_FREE, corner);
            if (!VHO_DSL_shadow_advance(&drain, free_service, VHO_DSL_SHADOW_ISSUE) ||
                !DSL_readiness_add(scalar_busy[sm], free_service, &scalar_busy[sm])) return FALSE;
            scalar[sm] = drain;
        }
        finished = VHO_DSL_shadow_max(finished, drain);
    }
    VHO_DSL_COMPUTE_SHADOW_COST result = {};
    result.kernel = finished.time;
    memcpy(result.terms, finished.terms, sizeof(result.terms));
    result.floor = DSL_readiness_ceil(device_bytes, device_rate);
    for (UINT32 sm = 0; sm < model.sms; ++sm) {
        result.floor = std::max(result.floor, compute_busy[sm]);
        result.floor = std::max(result.floor, scalar_busy[sm]);
        result.floor = std::max(result.floor, shared_busy[sm]);
        result.floor = std::max(result.floor, tmem_busy[sm]);
    }
    if (result.kernel < result.floor) return FALSE;
    result.operand_wait = operand_wait; result.accumulator_wait = accumulator_wait;
    result.queue_wait = queue_wait; result.resource_wait = resource_wait;
    *cost = result;
    return TRUE;
}

/* Build checked work/reservation facts before timing. Unknown model fields
 * suppress costs; malformed arithmetic and schedule budgets fail atomically. */
BOOL VHO_DSL_compute_shadow_analyze(const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, const DSL_COMPUTE_MODEL_RECORD *model,
    uint64_t event_budget, VHO_DSL_COMPUTE_SHADOW_RESULT *result, FILE *diagnostic)
{
    if (!result || !record || !links || !DSL_compute_model_verify(model) ||
        model->compute_profile_id != record->profile_id || model->arch_id != links->arch_id ||
        !DSL_compute_realization_verify(record, links, diagnostic)) return VHO_DSL_shadow_error(diagnostic, "invalid contract/model");
    DSL_COMPUTE_PROFILE_RECORD profile;
    DSL_compute_profile_get(record->profile_id, &profile);
    const DSL_TILE_PLAN_RECORD &tile = *links->tile;
    const bool simt = model->family == DSL_COMPUTE_SIMT;
    const bool tmem = !simt && profile.family == DSL_COMPUTE_TCGEN05;
    VHO_DSL_COMPUTE_SHADOW_RESULT output = {};
    output.family = model->family; output.numerical_contract_id = record->numerical_contract_id;
    output.input_format = links->numerical->input_format; output.owner_pu_st = record->owner_pu_st;
    output.region_id = record->region_id; output.generation = record->generation;
    output.scenario_id = model->scenario_id; output.unit = model->unit;
    output.source_identity_id = record->source_identity_id; output.semantic_node_id = record->semantic_node_id;
    output.problem_m = tile.problem_m; output.problem_n = tile.problem_n; output.problem_k = tile.problem_k;
    output.numerical = *links->numerical;
    uint64_t temporary, per_input, per_output, per_atom, per_cta_atoms;
    if (!DSL_readiness_mul(tile.problem_m, tile.problem_n, &temporary) ||
        !DSL_readiness_mul(temporary, tile.problem_k, &temporary) ||
        !DSL_readiness_mul(temporary, 2, &output.useful_flops) ||
        !DSL_readiness_mul(tile.problem_m / tile.cta_m, tile.problem_n / tile.cta_n, &output.ctas) ||
        !DSL_readiness_mul(output.ctas, tile.problem_k / profile.instruction_k, &output.atoms))
        return VHO_DSL_shadow_error(diagnostic, "work overflow");
    output.steps = tile.problem_k / tile.cta_k;
    per_cta_atoms = tile.problem_k / profile.instruction_k;
    per_input = (uint64_t)(tile.cta_m + tile.cta_n) * tile.cta_k * 2;
    per_output = (uint64_t)tile.cta_m * tile.cta_n * 4;
    per_atom = (uint64_t)2 * profile.instruction_m * profile.instruction_n * profile.instruction_k;
    if (!DSL_readiness_mul(output.atoms, per_atom, &output.executed_flops) ||
        !DSL_readiness_mul(output.ctas, output.steps, &temporary) ||
        !DSL_readiness_mul(temporary, per_input, &output.input_bytes) ||
        !DSL_readiness_mul(output.ctas, per_output, &output.output_bytes) ||
        !DSL_readiness_mul(output.input_bytes, 2, &output.shared_bytes))
        return VHO_DSL_shadow_error(diagnostic, "traffic overflow");
    output.tmem_bytes = tmem ? output.output_bytes : 0;
    if (!model->configured || (tmem && !model->tmem_cap_known)) { *result = output; return TRUE; }
    if (model->sms > VHO_DSL_SHADOW_MAX_SMS || model->queue_capacity > VHO_DSL_SHADOW_MAX_QUEUE)
        return VHO_DSL_shadow_error(diagnostic, "resource evaluation budget");
    uint64_t registers = links->resources->storage.register_bytes_per_thread;
    if (simt) registers = std::max(registers, (per_output / profile.threads_per_cta) + 8);
    DSL_GEMM_OCCUPANCY_RESULT residency;
    if (!DSL_gemm_occupancy(model->resources, 32, model->sms, profile.threads_per_cta, registers,
        links->resources->storage.shared_bytes, output.ctas, &residency) || !residency.resident)
        return VHO_DSL_shadow_error(diagnostic, "resource demand rejected");
    output.rounded_registers = residency.registers; output.rounded_shared = residency.shared;
    output.resident = std::min(residency.resident, (uint64_t)model->resident_cap);
    if (tmem) {
        output.resident = std::min(output.resident, (uint64_t)model->tmem_ctas_per_sm);
    }
    for (UINT32 index = 0; index < 5; ++index)
        if (residency.limits[index] == output.resident) output.limiting |= (uint64_t)1 << index;
    if (output.resident == model->resident_cap) output.limiting |= 32;
    if (tmem && output.resident == model->tmem_ctas_per_sm) output.limiting |= 64;
    uint64_t capacity;
    if (!DSL_readiness_mul(output.resident, model->sms, &capacity) || capacity > VHO_DSL_SHADOW_MAX_CTAS)
        return VHO_DSL_shadow_error(diagnostic, "CTA evaluation budget");
    output.full_rounds = output.ctas / capacity; output.tail_ctas = output.ctas % capacity;
    output.unused_slots = output.tail_ctas ? capacity - output.tail_ctas : 0;
    for (UINT32 metric = 0; metric < DSL_COMPUTE_MODEL_METRIC_COUNT; ++metric) {
        bool used = true;
        if (metric == DSL_COMPUTE_MODEL_TENSOR_FLOPS) used = !simt;
        if (metric == DSL_COMPUTE_MODEL_SCALAR_FLOPS || metric == DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS) used = simt;
        if (metric == DSL_COMPUTE_MODEL_TMEM_BYTES || metric == DSL_COMPUTE_MODEL_ALLOC || metric == DSL_COMPUTE_MODEL_FREE) used = tmem;
        if (used && !model->metrics[metric].known) { *result = output; return TRUE; }
    }
    uint64_t events;
    if (!DSL_readiness_add(output.steps, per_cta_atoms, &events) ||
        !DSL_readiness_add(events, 2, &events) || !DSL_readiness_mul(events, capacity, &events) ||
        !DSL_readiness_mul(events, 16, &events) || events > event_budget)
        return VHO_DSL_shadow_error(diagnostic, "event evaluation budget");
    for (UINT32 corner = 0; corner < 3; ++corner) {
        VHO_DSL_COMPUTE_SHADOW_COST &total = output.costs[corner];
        for (UINT32 tail = 0; tail < 2; ++tail) {
            uint64_t active = tail ? output.tail_ctas : capacity;
            uint64_t rounds = tail ? (active != 0) : output.full_rounds;
            if (!rounds) continue;
            VHO_DSL_COMPUTE_SHADOW_COST wave;
            if (!VHO_DSL_shadow_round(profile, *model, tile, active, corner, &wave))
                return VHO_DSL_shadow_error(diagnostic, "schedule arithmetic or floor failure");
            uint64_t *destinations[] = {&total.kernel, &total.floor, &total.operand_wait,
                &total.accumulator_wait, &total.queue_wait, &total.resource_wait};
            const uint64_t sources[] = {wave.kernel, wave.floor, wave.operand_wait,
                wave.accumulator_wait, wave.queue_wait, wave.resource_wait};
            for (UINT32 index = 0; index < 6; ++index)
                if (!DSL_readiness_mul(sources[index], rounds, &temporary) ||
                    !DSL_readiness_add(*destinations[index], temporary, destinations[index])) return VHO_DSL_shadow_error(diagnostic, "round overflow");
            for (UINT32 term = 0; term < VHO_DSL_SHADOW_TERM_COUNT; ++term)
                if (!DSL_readiness_mul(wave.terms[term], rounds, &temporary) ||
                    !DSL_readiness_add(total.terms[term], temporary, &total.terms[term])) return VHO_DSL_shadow_error(diagnostic, "attribution overflow");
        }
        if (!DSL_readiness_add(VHO_DSL_shadow_metric(*model, DSL_COMPUTE_MODEL_PREPARATION, corner),
                VHO_DSL_shadow_metric(*model, DSL_COMPUTE_MODEL_LAUNCH, corner), &temporary) ||
            !DSL_readiness_add(total.kernel, temporary, &total.invocation)) return VHO_DSL_shadow_error(diagnostic, "invocation overflow");
        total.terms[VHO_DSL_SHADOW_INVOCATION] = temporary;
    }
    if (output.costs[0].invocation > output.costs[1].invocation || output.costs[1].invocation > output.costs[2].invocation)
        return VHO_DSL_shadow_error(diagnostic, "nonmonotone scenario envelope");
    output.known = 1;
    *result = output;
    return TRUE;
}

/* Validate generated result envelopes before accepting a diagnostic comparison.
 * Neither consistency nor known timing constitutes lowerability evidence. */
static BOOL VHO_DSL_shadow_result_valid(const VHO_DSL_COMPUTE_SHADOW_RESULT &result)
{
    if (result.known > 1) return FALSE;
    if (!result.known) return TRUE;
    if (!result.scenario_id || result.unit != DSL_OPT_COST_UNIT_CYCLES || !result.resident) return FALSE;
    for (UINT32 corner = 0; corner < 3; ++corner) {
        const VHO_DSL_COMPUTE_SHADOW_COST &cost = result.costs[corner];
        uint64_t sum = 0;
        for (UINT32 term = 0; term < VHO_DSL_SHADOW_TERM_COUNT; ++term)
            if (!DSL_readiness_add(sum, cost.terms[term], &sum)) return FALSE;
        if (sum != cost.invocation || cost.floor > cost.kernel || cost.kernel > cost.invocation ||
            (corner && result.costs[corner - 1].invocation > cost.invocation)) return FALSE;
    }
    return TRUE;
}

/* Conditional ranking cannot promote execution, even for disjoint bounds.
 * Compare the complete numerical snapshot as well as PU/work identities. */
BOOL VHO_DSL_compute_shadow_compare(const VHO_DSL_COMPUTE_SHADOW_RESULT *simt,
    const VHO_DSL_COMPUTE_SHADOW_RESULT *mma, VHO_DSL_COMPUTE_SHADOW_COMPARISON *comparison)
{
    if (!simt || !mma || !comparison || simt->family != DSL_COMPUTE_SIMT ||
        (mma->family != DSL_COMPUTE_WGMMA && mma->family != DSL_COMPUTE_TCGEN05) ||
        simt->numerical_contract_id != mma->numerical_contract_id || simt->input_format != mma->input_format ||
        simt->owner_pu_st != mma->owner_pu_st || simt->region_id != mma->region_id ||
        simt->generation != mma->generation || simt->scenario_id != mma->scenario_id || simt->unit != mma->unit ||
        simt->source_identity_id != mma->source_identity_id || simt->semantic_node_id != mma->semantic_node_id ||
        simt->problem_m != mma->problem_m || simt->problem_n != mma->problem_n || simt->problem_k != mma->problem_k ||
        memcmp(&simt->numerical, &mma->numerical, sizeof(simt->numerical)) != 0 ||
        !VHO_DSL_shadow_result_valid(*simt) || !VHO_DSL_shadow_result_valid(*mma) ||
        simt->useful_flops != mma->useful_flops || simt->input_bytes != mma->input_bytes || simt->output_bytes != mma->output_bytes)
        return FALSE;
    VHO_DSL_COMPUTE_SHADOW_COMPARISON output = {};
    output.emitted_family = DSL_COMPUTE_SIMT;
    if (simt->known && mma->known) {
        output.nominal_family = mma->costs[1].invocation < simt->costs[1].invocation ? mma->family : (UINT32)DSL_COMPUTE_SIMT;
        if (mma->costs[2].invocation < simt->costs[0].invocation) output.bounded_family = mma->family;
        else if (simt->costs[2].invocation < mma->costs[0].invocation) output.bounded_family = DSL_COMPUTE_SIMT;
    }
    *comparison = output;
    return TRUE;
}

/* Print complete conditional inputs/results in a new stream, not AI_OPT 7.
 * Model inputs permit independent schedule replay; unknowns remain textual. */
BOOL VHO_DSL_compute_shadow_print(FILE *file, const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, const DSL_COMPUTE_MODEL_RECORD *model, uint64_t event_budget)
{
    VHO_DSL_COMPUTE_SHADOW_RESULT result;
    if (!file || !VHO_DSL_compute_shadow_analyze(record, links, model, event_budget, &result, NULL)) return FALSE;
    const DSL_TILE_PLAN_RECORD &tile = *links->tile;
    fprintf(file, "# What it summarizes: MMA2 conditional analytical schedule; no typed fallback, executable selection or native certification.\n");
    fprintf(file, "# AI_MMA_SHADOW 1 MODEL id profile family arch provenance unit scenario configured sms resident_cap queue tmem_known tmem_cap budget source_line\n");
    fprintf(file, "AI_MMA_SHADOW 1 MODEL %u %u %u %u %u %u %u %u %u %u %u %u %u %llu %u\n",
        model->id, model->compute_profile_id, model->family, model->arch_id, model->provenance,
        model->unit, model->scenario_id, model->configured, model->sms, model->resident_cap,
        model->queue_capacity, model->tmem_cap_known, model->tmem_ctas_per_sm, (unsigned long long)event_budget, record->source_line);
    fprintf(file, "# AI_MMA_SHADOW 1 GEOMETRY M N K BM BN BK registers shared source_line\n");
    fprintf(file, "AI_MMA_SHADOW 1 GEOMETRY %llu %llu %llu %u %u %u %u %llu %u\n",
        (unsigned long long)tile.problem_m, (unsigned long long)tile.problem_n, (unsigned long long)tile.problem_k,
        tile.cta_m, tile.cta_n, tile.cta_k, links->resources->storage.register_bytes_per_thread,
        (unsigned long long)links->resources->storage.shared_bytes, record->source_line);
    fprintf(file, "# AI_MMA_SHADOW 1 CAPACITY blocks warps threads block_threads registers thread_regs reg_quantum warp_quantum shared_capacity shared_budget shared_quantum shared_reserved\n");
    const DSL_HARDWARE_RESOURCE_RECORD &resource = model->resources;
    if (!model->configured) fprintf(file, "AI_MMA_SHADOW 1 CAPACITY unknown\n");
    else fprintf(file, "AI_MMA_SHADOW 1 CAPACITY %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
        (unsigned long long)resource.max_blocks, (unsigned long long)resource.max_warps, (unsigned long long)resource.max_threads,
        (unsigned long long)resource.max_threads_per_cta, (unsigned long long)resource.registers_per_sm,
        (unsigned long long)resource.max_registers_per_thread, (unsigned long long)resource.register_quantum,
        (unsigned long long)resource.register_warp_quantum, (unsigned long long)resource.shared_capacity,
        (unsigned long long)resource.shared_budget, (unsigned long long)resource.shared_quantum, (unsigned long long)resource.shared_reserved);
    fprintf(file, "# AI_MMA_SHADOW 1 METRIC ordinal lower nominal upper state\n");
    for (UINT32 index = 0; index < DSL_COMPUTE_MODEL_METRIC_COUNT; ++index) {
        const DSL_COMPUTE_MODEL_RANGE &range = model->metrics[index];
        if (!range.known) fprintf(file, "AI_MMA_SHADOW 1 METRIC %u unknown unknown unknown unknown\n", index);
        else fprintf(file, "AI_MMA_SHADOW 1 METRIC %u %llu %llu %llu synthetic\n", index,
            (unsigned long long)range.lower, (unsigned long long)range.nominal, (unsigned long long)range.upper);
    }
    fprintf(file, "# AI_MMA_SHADOW 1 WORK useful executed atoms ctas steps input output shared tmem source_line\n");
    fprintf(file, "AI_MMA_SHADOW 1 WORK %llu %llu %llu %llu %llu %llu %llu %llu %llu %u\n",
        (unsigned long long)result.useful_flops, (unsigned long long)result.executed_flops, (unsigned long long)result.atoms,
        (unsigned long long)result.ctas, (unsigned long long)result.steps, (unsigned long long)result.input_bytes,
        (unsigned long long)result.output_bytes, (unsigned long long)result.shared_bytes, (unsigned long long)result.tmem_bytes, record->source_line);
    fprintf(file, "# AI_MMA_SHADOW 1 RESIDENCY reg shared resident limiting full tail unused state source_line\n");
    if (!result.resident) fprintf(file, "AI_MMA_SHADOW 1 RESIDENCY unknown unknown unknown unknown unknown unknown unknown unknown %u\n", record->source_line);
    else fprintf(file, "AI_MMA_SHADOW 1 RESIDENCY %llu %llu %llu %llu %llu %llu %llu estimated %u\n",
        (unsigned long long)result.rounded_registers, (unsigned long long)result.rounded_shared,
        (unsigned long long)result.resident, (unsigned long long)result.limiting, (unsigned long long)result.full_rounds,
        (unsigned long long)result.tail_ctas, (unsigned long long)result.unused_slots, record->source_line);
    fprintf(file, "# AI_MMA_SHADOW 1 COST corner kernel invocation floor compute operand sync issue completion epilogue output invocation_setup operand_wait accumulator_wait queue_wait resource_wait source_line\n");
    for (UINT32 corner = 0; corner < 3; ++corner) {
        if (!result.known) fprintf(file, "AI_MMA_SHADOW 1 COST %u unknown %u\n", corner, record->source_line);
        else {
            const VHO_DSL_COMPUTE_SHADOW_COST &cost = result.costs[corner];
            fprintf(file, "AI_MMA_SHADOW 1 COST %u %llu %llu %llu", corner, (unsigned long long)cost.kernel,
                (unsigned long long)cost.invocation, (unsigned long long)cost.floor);
            for (UINT32 term = 0; term < VHO_DSL_SHADOW_TERM_COUNT; ++term) fprintf(file, " %llu", (unsigned long long)cost.terms[term]);
            fprintf(file, " %llu %llu %llu %llu %u\n", (unsigned long long)cost.operand_wait,
                (unsigned long long)cost.accumulator_wait, (unsigned long long)cost.queue_wait,
                (unsigned long long)cost.resource_wait, record->source_line);
        }
    }
    return !ferror(file);
}

/* Actual F32 joint planning cannot construct a typed MMA plan. Keep the
 * source numerical contract and emitted SIMT selection completely intact. */
void VHO_DSL_compute_shadow_trace_source(FILE *file, UINT32 arch_id, UINT32 format,
    UINT32 selected_simt, UINT32 source_line)
{
    if (!file) return;
    fprintf(file, "# AI_MMA_SHADOW 1 SOURCE profile arch format eligibility timing old_simt shadow emitted_simt action source_line\n");
    for (UINT32 profile_id = 1; profile_id <= 4; ++profile_id) {
        UINT32 reason = VHO_DSL_compute_shadow_eligibility(arch_id, format, profile_id);
        fprintf(file, "AI_MMA_SHADOW 1 SOURCE %u %u %u %s unknown %u unknown %u none %u\n", profile_id, arch_id, format,
            reason == VHO_DSL_SHADOW_TARGET ? "exact_target_required" : reason == VHO_DSL_SHADOW_NUMERICAL ? "numerical_contract_required" : "unlowered_contract",
            selected_simt, selected_simt, source_line);
    }
}
