/* Stable runtime scalar-format vocabulary shared by target descriptions and
 * backend numerical contracts. Owned by common/targ_info/access, not VHO plan
 * storage; these values are not new WHIRL MTYPE or binary image encodings.
 * Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
#ifndef dsl_compute_format_INCLUDED
#define dsl_compute_format_INCLUDED

typedef enum {
    DSL_ACC_FORMAT_UNKNOWN = 0,
    DSL_ACC_FORMAT_F32 = 1,
    DSL_ACC_FORMAT_F8_E4M3 = 2,
    DSL_ACC_FORMAT_F8_E5M2 = 3,
    DSL_ACC_FORMAT_F4_E2M1 = 4,
    /* Numerical format vocabulary shared by VHO compute contracts. The
     * segmented accumulation API below still rejects F16/BF16; the separate
     * typed-SIMT verifier accepts their explicit FP32 accumulation contract.
     * doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
    DSL_ACC_FORMAT_F16 = 5,
    DSL_ACC_FORMAT_BF16 = 6
} DSL_ACCUMULATION_FORMAT;

#endif
