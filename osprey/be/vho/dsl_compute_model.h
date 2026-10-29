/* VHO analytical-model ranges for conditional compute schedules.
 * These nonpersistent backend records supply no measured rates, binary image
 * fields or implementation-selection policy; unknown timing stays explicit.
 * Design: doc/AI-COMPILER-MMA2-SHADOW-MODEL.md. */
#ifndef dsl_compute_model_INCLUDED
#define dsl_compute_model_INCLUDED
#include "dsl_compute_realization.h"

typedef enum {
    DSL_COMPUTE_MODEL_UNKNOWN = 0,
    DSL_COMPUTE_MODEL_SYNTHETIC = 1
} DSL_COMPUTE_MODEL_PROVENANCE;

typedef enum {
    DSL_COMPUTE_MODEL_DEVICE_BYTES = 0,
    DSL_COMPUTE_MODEL_SHARED_BYTES = 1,
    DSL_COMPUTE_MODEL_TENSOR_FLOPS = 2,
    DSL_COMPUTE_MODEL_SCALAR_FLOPS = 3,
    DSL_COMPUTE_MODEL_EPILOGUE_ELEMENTS = 4,
    DSL_COMPUTE_MODEL_TMEM_BYTES = 5,
    DSL_COMPUTE_MODEL_CONVERSION_ELEMENTS = 6,
    DSL_COMPUTE_MODEL_GLOBAL_LATENCY = 7,
    DSL_COMPUTE_MODEL_ISSUE = 8,
    DSL_COMPUTE_MODEL_RESULT_LATENCY = 9,
    DSL_COMPUTE_MODEL_SYNC = 10,
    DSL_COMPUTE_MODEL_PROLOGUE = 11,
    DSL_COMPUTE_MODEL_ALLOC = 12,
    DSL_COMPUTE_MODEL_FREE = 13,
    DSL_COMPUTE_MODEL_PREPARATION = 14,
    DSL_COMPUTE_MODEL_LAUNCH = 15,
    DSL_COMPUTE_MODEL_METRIC_COUNT = 16
} DSL_COMPUTE_MODEL_METRIC;

typedef struct {
    UINT32 known;
    UINT32 reserved;
    uint64_t lower;
    uint64_t nominal;
    uint64_t upper;
} DSL_COMPUTE_MODEL_RANGE;

typedef struct {
    UINT32 id;
    UINT32 revision;
    DSL_COMPUTE_PROFILE_ID compute_profile_id;
    UINT32 family;
    UINT32 arch_id;
    UINT32 provenance;
    UINT32 unit;
    UINT32 scenario_id;
    UINT32 configured;
    UINT32 sms;
    UINT32 resident_cap;
    UINT32 queue_capacity;
    UINT32 tmem_cap_known;
    UINT32 tmem_ctas_per_sm;
    UINT32 reserved;
    DSL_HARDWARE_RESOURCE_RECORD resources;
    DSL_COMPUTE_MODEL_RANGE metrics[DSL_COMPUTE_MODEL_METRIC_COUNT];
} DSL_COMPUTE_MODEL_RECORD;

/* Exact recipe/family lookup returns an explicitly unknown description.
 * SIMT is a hypothetical typed companion, never a lowerable F32 fallback.
 * Failure preserves the caller's output and creates no persistent state. */
extern BOOL DSL_compute_model_unknown(DSL_COMPUTE_PROFILE_ID profile_id,
    UINT32 family, DSL_COMPUTE_MODEL_RECORD *model);

/* Check known-state/range/provenance consistency without interpreting costs
 * or selecting a resource configuration. Borrow the model without mutation;
 * return FALSE for invalid data. Unknown fields are not zero rates. */
extern BOOL DSL_compute_model_verify(const DSL_COMPUTE_MODEL_RECORD *model);
#endif
