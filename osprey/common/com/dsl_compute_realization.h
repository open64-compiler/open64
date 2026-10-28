/* Policy-free, caller-owned compute-realization inventory for one active PU.
 * Reuses common tile/plan, numerical and evolution identities. No image state,
 * candidate generation, timing, selection or emission is introduced.
 * Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md, MMA1. */
#ifndef dsl_compute_realization_INCLUDED
#define dsl_compute_realization_INCLUDED

#include "dsl_accumulation.h"
#include "dsl_fetch_pipeline.h"

typedef UINT32 DSL_COMPUTE_REALIZATION_ID;
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

/* An unresolved typed fallback requirement is distinct from an executable
 * OptimizationPlanIR. MMA3 must supply and verify the latter before lowering. */
typedef struct {
    UINT32 id;
    UINT32 family;
    UINT32 numerical_contract_id;
    UINT32 source_format;
    UINT32 output_format;
    UINT32 support;
    DSL_OPT_PLAN_ID plan_id;
    UINT64 generation;
    UINT32 reserved;
} DSL_COMPUTE_FALLBACK_RECORD;

/* Required event references only. These are not an event graph or a proof of
 * completion/progress. Existing copy-ready stages cannot represent MMA done. */
typedef struct {
    UINT32 id;
    UINT32 recipe;
    UINT32 obligations;
    UINT32 operand_ready;
    UINT32 issue;
    UINT32 completion;
    UINT32 result_read;
    UINT32 reuse_join;
    UINT32 allocation;
    UINT32 deallocation;
    UINT32 permit_release;
    UINT32 outstanding_groups;
    UINT32 reserved;
} DSL_COMPUTE_PROTOCOL_RECORD;

/* Extension beside the existing RF/SMEM ledger; TMEM is neither RF nor SMEM.
 * Quantities are caller-supplied planning requirements, not native allocations. */
typedef struct {
    DSL_FETCH_RESOURCE_RECORD storage;
    UINT32 tmem_lanes;
    UINT32 tmem_columns;
    UINT32 tmem_allocation_columns;
    UINT32 allocation_scope;
    UINT32 reserved;
} DSL_COMPUTE_RESOURCE_RECORD;

typedef struct {
    DSL_COMPUTE_REALIZATION_ID id;
    UINT32 version;
    ST_IDX owner_pu_st;
    DSL_PU_SOURCE_IDENTITY_ID source_identity_id;
    UINT32 region_id;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_OPT_PLAN_ID plan_id;
    DSL_TILE_PLAN_ID tile_plan_id;
    DSL_COMPUTE_PROFILE_ID profile_id;
    UINT32 descriptor_recipe;
    UINT32 a_source_tier;
    UINT32 b_source_tier;
    UINT32 fragment_mapping;
    DSL_TENSOR_EVOLUTION_NODE_ID operand_nodes[2];
    DSL_TENSOR_EVOLUTION_NODE_ID accumulator_node;
    DSL_TENSOR_EVOLUTION_NODE_ID result_node;
    UINT32 numerical_contract_id;
    UINT32 numerical_acceptance_id;
    UINT32 protocol_id;
    UINT32 fallback_requirement_id;
    UINT64 generation;
    UINT32 required_ptx;
    UINT32 required_toolkit;
    UINT32 lowering_support;
    UINT32 native_support;
    UINT32 reserved;
    UINT32 source_line;
} DSL_COMPUTE_REALIZATION_RECORD;

/* All references are borrowed for this call, in driver-established PU scope.
 * Four evolution nodes are A, B, accumulator, result; descriptor IDs retain
 * real tensor identities. Full descriptor/alias/ISA proofs belong to MMA4. */
typedef struct {
    ST_IDX owner_pu_st;
    UINT32 arch_id;
    UINT32 region_id;
    DSL_PU_SOURCE_IDENTITY_ID source_identity_id;
    UINT64 generation;
    UINT32 numerical_acceptance_id;
    const DSL_TILE_SITE_RECORD *site;
    const DSL_TILE_PLAN_RECORD *tile;
    const DSL_OPT_PLAN_RECORD *plan;
    const DSL_TENSOR_EVOLUTION_NODE_RECORD *representations;
    UINT32 representation_count;
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical;
    const DSL_COMPUTE_PROTOCOL_RECORD *protocol;
    const DSL_COMPUTE_RESOURCE_RECORD *resources;
    const DSL_COMPUTE_FALLBACK_RECORD *fallback;
} DSL_COMPUTE_REALIZATION_LINKS;

/* Return an exact immutable inventory row by ID; failure preserves output. */
extern BOOL DSL_compute_profile_get(DSL_COMPUTE_PROFILE_ID id,
    DSL_COMPUTE_PROFILE_RECORD *profile);

/* Exact architecture and concrete recipe spelling; never strip suffixes,
 * infer another architecture, or equate lookup success with emission support. */
extern BOOL DSL_compute_profile_find(const char *architecture,
    const char *variant, DSL_COMPUTE_PROFILE_RECORD *profile);

/* Structural joins, inventory restrictions and generation validation only.
 * Returns no selected plan and mutates no inputs or WHIRL on any path. */
extern BOOL DSL_compute_realization_verify(
    const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, FILE *diagnostic);

/* Separate versioned inspection stream, never AI_OPT/model6 reinterpretation.
 * Invalid records produce no stream. Candidate source line remains last. */
extern BOOL DSL_compute_realization_print(FILE *file,
    const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links);

#endif
