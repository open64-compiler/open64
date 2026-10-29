/* VHO analytical description construction and structural verification.
 * Scheduling remains in dsl_compute_shadow_opt.cxx; neither service changes
 * WHIRL, emits code, nor converts unknown timing into measured/synthetic data.
 * Design: doc/AI-COMPILER-MMA2-SHADOW-MODEL.md. */
#include "dsl_compute_model.h"
#include <string.h>

/* Build an explicitly unknown description from an exact profile/family
 * match. Copy to the borrowed output only on success; null output, missing
 * profile or incompatible family returns FALSE without changing the output.
 * No private Tensor Core rates or synthetic defaults enter target lookup. */
BOOL DSL_compute_model_unknown(DSL_COMPUTE_PROFILE_ID profile_id,
    UINT32 family, DSL_COMPUTE_MODEL_RECORD *model)
{
    DSL_COMPUTE_PROFILE_RECORD profile;
    if (!model || !DSL_compute_profile_get(profile_id, &profile) ||
        (family != DSL_COMPUTE_SIMT && family != profile.family)) return FALSE;
    DSL_COMPUTE_MODEL_RECORD result = {};
    result.id = profile_id;
    result.revision = 1;
    result.compute_profile_id = profile_id;
    result.family = family;
    result.arch_id = profile.arch_id;
    *model = result;
    return TRUE;
}

/* A known duration can be zero; a known service rate must be positive.
 * Synthetic provenance is mandatory for all supplied timing in MMA2.
 * Read the caller-owned model without mutation and return FALSE for malformed
 * profile/configuration joins, unsupported provenance or inconsistent ranges. */
BOOL DSL_compute_model_verify(const DSL_COMPUTE_MODEL_RECORD *model)
{
    DSL_COMPUTE_PROFILE_RECORD profile;
    if (!model || !model->id || model->revision != 1 || model->reserved ||
        !DSL_compute_profile_get(model->compute_profile_id, &profile) ||
        model->arch_id != profile.arch_id ||
        (model->family != DSL_COMPUTE_SIMT && model->family != profile.family) ||
        model->provenance > DSL_COMPUTE_MODEL_SYNTHETIC || model->configured > 1 || model->tmem_cap_known > 1)
        return FALSE;
    if (model->provenance == DSL_COMPUTE_MODEL_UNKNOWN) {
        if (model->unit || model->scenario_id || model->configured || model->sms || model->resident_cap ||
            model->queue_capacity || model->tmem_cap_known || model->tmem_ctas_per_sm) return FALSE;
    } else if (model->unit != DSL_OPT_COST_UNIT_CYCLES || !model->scenario_id) return FALSE;
    if (model->configured && (!model->sms || !model->resident_cap || !model->queue_capacity ||
        !DSL_hardware_resources_valid(model->resources))) return FALSE;
    if (!model->configured && (model->sms || model->resident_cap || model->queue_capacity)) return FALSE;
    if (model->tmem_cap_known != (model->tmem_ctas_per_sm != 0)) return FALSE;
    for (UINT32 index = 0; index < DSL_COMPUTE_MODEL_METRIC_COUNT; ++index) {
        const DSL_COMPUTE_MODEL_RANGE &range = model->metrics[index];
        if (range.known > 1 || range.reserved || range.upper == ~(UINT64)0) return FALSE;
        if (!range.known) {
            if (range.lower || range.nominal || range.upper) return FALSE;
        } else if (model->provenance != DSL_COMPUTE_MODEL_SYNTHETIC ||
            range.lower > range.nominal || range.nominal > range.upper ||
            (index <= DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS && !range.lower)) return FALSE;
    }
    return TRUE;
}
