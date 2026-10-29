/* Immutable compute-instruction profile vocabulary and exact lookup contract.
 * Owned by common/targ_info/access; no PU, optimizer plan or backend header
 * dependencies, timing estimates, selection, emission or .B image fields.
 * Support labels describe the bounded inventory, not provider availability.
 * Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
#ifndef dsl_compute_profile_INCLUDED
#define dsl_compute_profile_INCLUDED

#include "defs.h"
#include "dsl_compute_format.h"

typedef UINT32 DSL_COMPUTE_PROFILE_ID;

typedef enum {
    DSL_COMPUTE_SIMT = 1,
    DSL_COMPUTE_WGMMA = 2,
    DSL_COMPUTE_TCGEN05 = 3
} DSL_COMPUTE_FAMILY;

typedef enum {
    DSL_COMPUTE_REQUIRED = 1,
    DSL_COMPUTE_IMPLEMENTED = 2,
    DSL_COMPUTE_NATIVE_CERTIFIED = 3
} DSL_COMPUTE_SUPPORT;

typedef enum {
    DSL_COMPUTE_FRAGMENT_WGMMA_64_64 = 1,
    DSL_COMPUTE_FRAGMENT_TCGEN05_128_64 = 2
} DSL_COMPUTE_FRAGMENT;

typedef enum {
    DSL_COMPUTE_ACCUMULATOR_RF = 1,
    DSL_COMPUTE_ACCUMULATOR_TMEM = 2
} DSL_COMPUTE_ACCUMULATOR_STORAGE;

enum {
    DSL_COMPUTE_OBLIGATION_OPERAND_READY = 1,
    DSL_COMPUTE_OBLIGATION_PROXY = 2,
    DSL_COMPUTE_OBLIGATION_REGISTER_FENCE = 4,
    DSL_COMPUTE_OBLIGATION_COMMIT = 8,
    DSL_COMPUTE_OBLIGATION_MMA_WAIT = 16,
    DSL_COMPUTE_OBLIGATION_RESULT_READ = 32,
    DSL_COMPUTE_OBLIGATION_REUSE_JOIN = 64,
    DSL_COMPUTE_OBLIGATION_TMEM_ALLOC = 128,
    DSL_COMPUTE_OBLIGATION_TMEM_FREE = 256,
    DSL_COMPUTE_OBLIGATION_PERMIT_RELEASE = 512,
    DSL_COMPUTE_OBLIGATION_LOAD_WAIT = 1024,
    DSL_COMPUTE_OBLIGATION_THREAD_SYNC = 2048
};

typedef struct {
    DSL_COMPUTE_PROFILE_ID id;
    UINT32 revision;
    UINT32 arch_id;
    const char *architecture;
    const char *variant;
    UINT32 family;
    UINT32 input_format;
    UINT32 accumulator_format;
    UINT32 instruction_m;
    UINT32 instruction_n;
    UINT32 instruction_k;
    UINT32 cta_group;
    UINT32 threads_per_cta;
    UINT32 mma_issuers;
    UINT32 allocation_threads;
    UINT32 fragment_mapping;
    UINT32 descriptor_recipe;
    UINT32 accumulator_tier;
    UINT32 accumulator_registers_per_thread;
    UINT32 tmem_lanes;
    UINT32 tmem_columns;
    UINT32 tmem_min_allocation;
    UINT32 tmem_max_allocation;
    UINT32 descriptor_alignment;
    UINT32 minimum_ptx;
    UINT32 minimum_toolkit;
    UINT32 evidence_ptx;
    UINT32 evidence_toolkit;
    UINT32 completion_recipe;
    UINT32 required_obligations;
    UINT32 lowering_support;
    UINT32 native_support;
} DSL_COMPUTE_PROFILE_RECORD;

/* Copy a complete immutable inventory row by one-based ID to caller storage.
 * Null output or unknown ID returns FALSE without mutation; string fields
 * remain borrowed static data. Lookup does not certify executable support. */
extern BOOL DSL_compute_profile_get(DSL_COMPUTE_PROFILE_ID id,
    DSL_COMPUTE_PROFILE_RECORD *profile);

/* Exact architecture and concrete recipe spelling; never strip suffixes,
 * infer another architecture, or equate lookup success with emission support.
 * Null arguments or absent pair returns FALSE without changing caller output;
 * successful copies borrow static strings and allocate no storage. */
extern BOOL DSL_compute_profile_find(const char *architecture,
    const char *variant, DSL_COMPUTE_PROFILE_RECORD *profile);

#endif
