/* PU-local MMA2 conditional schedule and diagnostic ranking. This API never
 * mutates executable WHIRL or supplies typed lowering/native certification.
 * Design: doc/AI-COMPILER-MMA2-SHADOW-MODEL.md. */
#ifndef dsl_compute_shadow_opt_INCLUDED
#define dsl_compute_shadow_opt_INCLUDED
#include "dsl_compute_model.h"

typedef enum {
    VHO_DSL_SHADOW_TARGET = 1,
    VHO_DSL_SHADOW_NUMERICAL = 2,
    VHO_DSL_SHADOW_COMPATIBLE = 3
} VHO_DSL_COMPUTE_SHADOW_ELIGIBILITY;

typedef enum {
    VHO_DSL_SHADOW_COMPUTE = 0,
    VHO_DSL_SHADOW_OPERAND = 1,
    VHO_DSL_SHADOW_SYNC = 2,
    VHO_DSL_SHADOW_ISSUE = 3,
    VHO_DSL_SHADOW_COMPLETION = 4,
    VHO_DSL_SHADOW_EPILOGUE = 5,
    VHO_DSL_SHADOW_OUTPUT = 6,
    VHO_DSL_SHADOW_INVOCATION = 7,
    VHO_DSL_SHADOW_TERM_COUNT = 8
} VHO_DSL_COMPUTE_SHADOW_TERM;

typedef struct {
    uint64_t kernel, invocation, floor;
    uint64_t terms[VHO_DSL_SHADOW_TERM_COUNT];
    uint64_t operand_wait, accumulator_wait, queue_wait, resource_wait;
} VHO_DSL_COMPUTE_SHADOW_COST;

typedef struct {
    UINT32 known;
    UINT32 family;
    UINT32 numerical_contract_id;
    UINT32 input_format;
    ST_IDX owner_pu_st;
    UINT32 region_id;
    UINT32 scenario_id;
    UINT32 unit;
    uint64_t generation;
    UINT32 source_identity_id, semantic_node_id;
    uint64_t problem_m, problem_n, problem_k;
    DSL_NUMERICAL_CONTRACT_RECORD numerical;
    uint64_t useful_flops, executed_flops, atoms, ctas, steps;
    uint64_t input_bytes, output_bytes, shared_bytes, tmem_bytes;
    uint64_t rounded_registers, rounded_shared, resident, limiting;
    uint64_t full_rounds, tail_ctas, unused_slots;
    VHO_DSL_COMPUTE_SHADOW_COST costs[3];
} VHO_DSL_COMPUTE_SHADOW_RESULT;

typedef struct {
    UINT32 nominal_family;
    UINT32 bounded_family;
    UINT32 emitted_family;
} VHO_DSL_COMPUTE_SHADOW_COMPARISON;

/* Screen exact target and source format, shared by real F32 trace consumers
 * and typed shadow fixtures. This is not a proof of complete legality. */
extern UINT32 VHO_DSL_compute_shadow_eligibility(UINT32 arch_id, UINT32 format,
    DSL_COMPUTE_PROFILE_ID profile_id);

/* Analyze a caller-owned MMA1 view or hypothetical same-contract SIMT
 * companion. Unknown data returns known=0; malformed/overflow/budget failure
 * preserves output. Scheduling never changes WN, ST, TY or plan selection. */
extern BOOL VHO_DSL_compute_shadow_analyze(const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, const DSL_COMPUTE_MODEL_RECORD *model,
    uint64_t event_budget, VHO_DSL_COMPUTE_SHADOW_RESULT *result, FILE *diagnostic);

/* Compare only identical numerical/work/scope/scenario boundaries. Bounds
 * are scenario envelopes; emitted execution is always SIMT, even if MMA wins. */
extern BOOL VHO_DSL_compute_shadow_compare(const VHO_DSL_COMPUTE_SHADOW_RESULT *simt,
    const VHO_DSL_COMPUTE_SHADOW_RESULT *mma, VHO_DSL_COMPUTE_SHADOW_COMPARISON *comparison);

/* Separate versioned inspection, with explicit inputs/unknowns and no action
 * claims. Invalid records produce no output. Candidate source line is last. */
extern BOOL VHO_DSL_compute_shadow_print(FILE *file, const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, const DSL_COMPUTE_MODEL_RECORD *model,
    uint64_t event_budget);

/* Real compiler F32 tracing records rejection/unknown costs only; no new
 * option or numerical conversion, and no effect on the selected SIMT ID. */
extern void VHO_DSL_compute_shadow_trace_source(FILE *file, UINT32 arch_id,
    UINT32 format, UINT32 selected_simt, UINT32 source_line);
#endif
