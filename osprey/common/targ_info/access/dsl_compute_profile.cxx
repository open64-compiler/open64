/* Common targ_info/access bounded immutable compute profiles and exact lookup.
 * Preserve ISA/toolkit evidence and conservative support labels; no rate model,
 * mutable compiler state, PU joins, lowering decision or WHIRL image changes.
 * Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
#include "dsl_compute_profile.h"
#include "dsl_memory_hierarchy.h"
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

/* Copy one complete bounded inventory row to caller storage. Null output
 * or zero/out-of-range ID returns FALSE without mutation; strings remain
 * borrowed from static storage. No code-generation capability is inferred. */
BOOL DSL_compute_profile_get(DSL_COMPUTE_PROFILE_ID id,
    DSL_COMPUTE_PROFILE_RECORD *profile)
{
    if (profile == NULL || id == 0 || id > sizeof(DSL_compute_profiles) / sizeof(DSL_compute_profiles[0]))
        return FALSE;
    *profile = DSL_compute_profiles[id - 1];
    return TRUE;
}

/* Match both complete case-sensitive names; suffixes are significant.
 * Copy a known row with borrowed strings on success; null input/output or
 * missing pair returns FALSE without mutation or architecture-family inference. */
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
